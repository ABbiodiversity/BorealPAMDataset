"""Clip downloaded audio and produce a mirrored tree of FLAC release files.

The default clip length is 180 seconds. The output directory also receives an
audio_processing_log.csv file. Input and output directories must be external to
the code repository.
"""

from __future__ import annotations

import argparse
import csv
import shutil
import subprocess
from pathlib import Path


SUPPORTED_EXTENSIONS = {".flac", ".mp3"}


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Recursively clip MP3 and FLAC recordings and write a mirrored "
            "directory tree containing FLAC files."
        )
    )
    parser.add_argument("input_directory", type=Path)
    parser.add_argument("output_directory", type=Path)
    parser.add_argument("--clip-seconds", type=float, default=180.0)
    parser.add_argument("--ffmpeg", default="ffmpeg")
    parser.add_argument("--ffprobe", default="ffprobe")
    parser.add_argument("--overwrite", action="store_true")
    return parser.parse_args()


def require_executable(command: str) -> None:
    if Path(command).is_file():
        return
    if shutil.which(command) is None:
        raise RuntimeError(f"Required executable was not found: {command}")


def audio_duration(path: Path, ffprobe: str) -> float:
    result = subprocess.run(
        [
            ffprobe,
            "-v",
            "error",
            "-show_entries",
            "format=duration",
            "-of",
            "default=noprint_wrappers=1:nokey=1",
            str(path),
        ],
        check=True,
        capture_output=True,
        text=True,
    )
    return float(result.stdout.strip())


def encode_flac(
    input_path: Path,
    output_path: Path,
    clip_seconds: float,
    ffmpeg: str,
) -> None:
    result = subprocess.run(
        [
            ffmpeg,
            "-hide_banner",
            "-loglevel",
            "error",
            "-nostdin",
            "-y",
            "-i",
            str(input_path),
            "-map",
            "0:a:0",
            "-t",
            str(clip_seconds),
            "-c:a",
            "flac",
            str(output_path),
        ],
        check=False,
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        raise RuntimeError(
            result.stderr.strip() or f"FFmpeg exited with code {result.returncode}"
        )


def main() -> int:
    arguments = parse_arguments()
    input_directory = arguments.input_directory.resolve(strict=True)
    output_directory = arguments.output_directory.resolve(strict=False)

    if not input_directory.is_dir():
        raise NotADirectoryError(input_directory)
    if input_directory == output_directory:
        raise ValueError("Input and output directories must be different.")
    if arguments.clip_seconds <= 0:
        raise ValueError("--clip-seconds must be greater than zero.")

    require_executable(arguments.ffmpeg)
    require_executable(arguments.ffprobe)
    output_directory.mkdir(parents=True, exist_ok=True)

    audio_files = sorted(
        path
        for path in input_directory.rglob("*")
        if path.is_file()
        and path.suffix.lower() in SUPPORTED_EXTENSIONS
        and output_directory not in path.parents
    )

    results: list[dict[str, object]] = []

    for index, input_path in enumerate(audio_files, start=1):
        relative_path = input_path.relative_to(input_directory)
        output_path = (output_directory / relative_path).with_suffix(".flac")
        output_path.parent.mkdir(parents=True, exist_ok=True)
        print(f"[{index}/{len(audio_files)}] {relative_path}")

        if output_path.exists() and not arguments.overwrite:
            results.append(
                {
                    "input_file": str(input_path),
                    "output_file": str(output_path),
                    "input_duration_seconds": "",
                    "output_duration_seconds": "",
                    "status": "skipped_existing",
                    "error": "",
                }
            )
            continue

        input_duration: float | str = ""
        output_duration: float | str = ""
        status = "failed"
        error = ""

        try:
            input_duration = audio_duration(input_path, arguments.ffprobe)

            if (
                input_path.suffix.lower() == ".flac"
                and input_duration <= arguments.clip_seconds
            ):
                shutil.copy2(input_path, output_path)
                status = "copied_flac"
            else:
                encode_flac(
                    input_path,
                    output_path,
                    arguments.clip_seconds,
                    arguments.ffmpeg,
                )
                status = "converted" if input_path.suffix.lower() == ".mp3" else "clipped"

            output_duration = audio_duration(output_path, arguments.ffprobe)
            if output_duration <= 0:
                raise RuntimeError("Output duration is zero.")
            if output_duration > arguments.clip_seconds + 0.5:
                raise RuntimeError(
                    f"Output duration {output_duration:.3f}s exceeds the clip limit."
                )
        except Exception as exception:  # Continue so every file is represented in the log.
            error = str(exception)
            status = "failed"
            if output_path.exists():
                output_path.unlink()

        results.append(
            {
                "input_file": str(input_path),
                "output_file": str(output_path),
                "input_duration_seconds": input_duration,
                "output_duration_seconds": output_duration,
                "status": status,
                "error": error,
            }
        )

    log_path = output_directory / "audio_processing_log.csv"
    with log_path.open("w", newline="", encoding="utf-8") as log_file:
        writer = csv.DictWriter(
            log_file,
            fieldnames=[
                "input_file",
                "output_file",
                "input_duration_seconds",
                "output_duration_seconds",
                "status",
                "error",
            ],
        )
        writer.writeheader()
        writer.writerows(results)

    failures = sum(result["status"] == "failed" for result in results)
    print(f"Processed {len(results)} files with {failures} failure(s).")
    print(f"Log: {log_path}")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
