"""Clip downloaded MP3/FLAC recordings and create FLAC release files.

This consolidates the three original audio-processing programs. It recursively
finds MP3 and FLAC recordings, clips recordings longer than 180 seconds,
converts MP3 files to FLAC, preserves the folder structure, skips existing
outputs, and writes a processing log.
"""

import argparse
import csv
import shutil
import subprocess
from pathlib import Path

from tqdm import tqdm


# ==========================================
# COMMAND-LINE CONFIGURATION
# ==========================================

parser = argparse.ArgumentParser()
parser.add_argument("input_directory", type=Path)
parser.add_argument("output_directory", type=Path)
parser.add_argument("--clip-length", type=float, default=180.0)
parser.add_argument("--ffmpeg", default="ffmpeg")
parser.add_argument("--ffprobe", default="ffprobe")
args = parser.parse_args()

INPUT_DIR = args.input_directory
OUTPUT_DIR = args.output_directory
CLIP_LENGTH = args.clip_length
FFMPEG = args.ffmpeg
FFPROBE = args.ffprobe

RECURSIVE = True
SKIP_EXISTING = True


# ==========================================
# GET AUDIO DURATION
# ==========================================

def get_duration(path):
    command = [
        FFPROBE,
        "-v", "error",
        "-show_entries", "format=duration",
        "-of", "default=noprint_wrappers=1:nokey=1",
        str(path),
    ]

    try:
        result = subprocess.run(
            command,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        return float(result.stdout.strip())
    except Exception:
        return None


# ==========================================
# CLIP AND/OR CONVERT TO FLAC
# ==========================================

def make_flac(input_path, output_path, clip_length):
    command = [
        FFMPEG,
        "-hide_banner",
        "-loglevel", "error",
        "-nostdin",
        "-y",
        "-ss", "0",
        "-t", str(int(clip_length)),
        "-i", str(input_path),
        "-map", "0:a:0",
        "-c:a", "flac",
        str(output_path),
    ]

    try:
        subprocess.run(
            command,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        return True, None
    except subprocess.CalledProcessError as error:
        return False, error.stderr.decode("utf-8", errors="ignore")
    except Exception as error:
        return False, str(error)


# ==========================================
# SETUP
# ==========================================

OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

search_pattern = INPUT_DIR.rglob("*") if RECURSIVE else INPUT_DIR.iterdir()

audio_files = sorted(
    path
    for path in search_pattern
    if path.is_file()
    and path.suffix.lower() in {".mp3", ".flac"}
    and OUTPUT_DIR not in path.parents
)

print(f"Input directory:  {INPUT_DIR}")
print(f"Output directory: {OUTPUT_DIR}")
print(f"Audio files found: {len(audio_files)}")


# ==========================================
# PROCESS FILES
# ==========================================

log = []

for input_path in tqdm(audio_files, desc="Processing audio files", unit="file"):
    relative_path = input_path.relative_to(INPUT_DIR)

    # All release files use the FLAC extension.
    output_path = (OUTPUT_DIR / relative_path).with_suffix(".flac")
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Skip files already processed.
    if SKIP_EXISTING and output_path.exists():
        log.append((str(relative_path), "skipped_existing", ""))
        continue

    duration = get_duration(input_path)

    if duration is None:
        log.append((str(relative_path), "error", "ffprobe_failed"))
        continue

    # Existing FLAC files at or below 180 seconds can be copied directly.
    if input_path.suffix.lower() == ".flac" and duration <= CLIP_LENGTH:
        shutil.copy2(input_path, output_path)
        log.append((str(relative_path), "copied_short_flac", duration))
        continue

    # Longer FLAC files are clipped and MP3 files are converted to FLAC. The
    # -t option also limits short MP3s without extending their duration.
    ok, error = make_flac(input_path, output_path, CLIP_LENGTH)

    if ok:
        new_duration = get_duration(output_path)
        action = "converted_mp3" if input_path.suffix.lower() == ".mp3" else "clipped_flac"
        log.append((str(relative_path), action, new_duration))
    else:
        log.append((str(relative_path), "failed", error))


# ==========================================
# SAVE LOG
# ==========================================

log_path = OUTPUT_DIR / "audio_processing_log.csv"

with open(log_path, "w", newline="", encoding="utf-8") as log_file:
    writer = csv.writer(log_file)
    writer.writerow(["file", "status", "detail"])
    writer.writerows(log)

print(f"Done. Log saved to {log_path}")

