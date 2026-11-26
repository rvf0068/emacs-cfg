"""Optional CI cache of pinned straight Git repositories, not working trees.

Only repositories at the recorded lockfile commit are eligible. Cached
repositories are cloned into the temporary home with a fresh checkout;
the cache is never used as an Emacs load path. Missing or unusable
entries fall back to normal straight cloning.
"""

import argparse
import json
import pathlib
import re
import shutil
import subprocess
import tempfile


def git(repo, *args):
    return subprocess.check_output(
        ["git", "-C", str(repo), *args], text=True, stderr=subprocess.DEVNULL
    ).strip()


def pins(path):
    return dict(re.findall(
        r'\("([^"/]+)"\s*\.\s*"([0-9a-f]{40})"\)', path.read_text()
    ))


def public_url(url):
    # CI only clones public upstream sources. Do not save authenticated URLs,
    # local paths, or arbitrary remote helper commands.
    return bool(re.fullmatch(r"https://[^/@\s]+/[^\s?#@]+", url))


def save(repos, cache, locked):
    cache.mkdir(parents=True, exist_ok=True)
    for name, commit in locked.items():
        src = repos / name
        dst = cache / name
        if dst.exists() or not (src / ".git").is_dir():
            continue
        try:
            url = git(src, "remote", "get-url", "origin")
            if not public_url(url) or git(src, "rev-parse", "HEAD") != commit:
                continue
            if git(src, "ls-tree", "--name-only", "HEAD", ".gitmodules"):
                continue  # Let straight initialize submodules normally.
            with tempfile.TemporaryDirectory(dir=cache) as tmp:
                entry = pathlib.Path(tmp) / "entry"
                entry.mkdir()
                git(src, "clone", "--local", "--no-hardlinks", "--bare",
                    str(src), str(entry / "repo.git"))
                (entry / "metadata.json").write_text(
                    json.dumps({"commit": commit, "url": url})
                )
                entry.rename(dst)
            print(f"Cached pinned source: {name}", flush=True)
        except (OSError, subprocess.CalledProcessError) as exc:
            print(f"Skipping source cache for {name}: {exc}", flush=True)


def restore(repos, cache, locked):
    if not cache.is_dir():
        return
    for name, commit in locked.items():
        entry = cache / name
        dst = repos / name
        if dst.exists() or not (entry / "repo.git").is_dir():
            continue
        try:
            metadata = json.loads((entry / "metadata.json").read_text())
            url = metadata["url"]
            if metadata["commit"] != commit or not public_url(url):
                continue
            if git(entry / "repo.git", "rev-parse", "HEAD") != commit:
                continue
            # Clone and checkout without reusing built .elc/.eln files,
            # generated sources, local Git hooks or test-home paths.
            with tempfile.TemporaryDirectory(dir=repos) as tmp:
                work = pathlib.Path(tmp) / "checkout"
                git(repos, "clone", "--local", "--no-hardlinks", "--no-checkout",
                    str(entry / "repo.git"), str(work))
                git(work, "remote", "set-url", "origin", url)
                git(work, "checkout", "--detach", commit)
                if git(work, "rev-parse", "HEAD") != commit:
                    raise ValueError("checkout did not match pin")
                work.rename(dst)
            print(f"Restored pinned source: {name}", flush=True)
        except (OSError, ValueError, KeyError, subprocess.CalledProcessError) as exc:
            print(f"Ignoring unusable source cache for {name}: {exc}", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("operation", choices=("save", "restore"))
    parser.add_argument("repos", type=pathlib.Path)
    parser.add_argument("cache", type=pathlib.Path)
    parser.add_argument("lockfile", type=pathlib.Path)
    args = parser.parse_args()
    locked = pins(args.lockfile)
    if not locked:
        parser.error("no package pins found")
    operation = save if args.operation == "save" else restore
    operation(args.repos, args.cache, locked)


if __name__ == "__main__":
    main()
