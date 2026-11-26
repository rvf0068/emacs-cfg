"""Offline integration checks for the optional Git source cache."""

import contextlib
import importlib.util
import io
import pathlib
import subprocess
import tempfile
import unittest


spec = importlib.util.spec_from_file_location(
    "source_cache", pathlib.Path(__file__).with_name("straight-source-cache.py")
)
cache_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(cache_module)


def git(repo, *args):
    return subprocess.check_output(
        ["git", "-C", str(repo), *args], text=True, stderr=subprocess.DEVNULL
    ).strip()


class SourceCacheTests(unittest.TestCase):
    def test_pinned_sources_are_restored_without_generated_files(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            repos = root / "repos"
            source = repos / "fixture"
            source.mkdir(parents=True)
            git(source, "init", "-q")
            git(source, "config", "user.email", "test@example.invalid")
            git(source, "config", "user.name", "CI fixture")
            (source / "fixture.el").write_text("original")
            git(source, "add", "fixture.el")
            git(source, "commit", "-qm", "fixture")
            commit = git(source, "rev-parse", "HEAD")
            git(source, "checkout", "--detach", commit)
            url = "https://example.invalid/fixture.git"
            git(source, "remote", "add", "origin", url)
            (source / "fixture.el").write_text("modified")
            (source / "generated.elc").write_text("not a source file")
            lock = root / "default.el"
            lock.write_text(f'(("fixture" . "{commit}"))\n:epsilon\n')
            pinned = cache_module.pins(lock)
            self.assertEqual(pinned, {"fixture": commit})
            cache = root / "cache"
            cache_module.save(repos, cache, pinned)
            self.assertTrue((cache / "fixture" / "repo.git").is_dir())
            fresh = root / "fresh"
            fresh.mkdir()
            cache_module.restore(fresh, cache, pinned)
            restored = fresh / "fixture"
            self.assertEqual(git(restored, "rev-parse", "HEAD"), commit)
            self.assertEqual(git(restored, "remote", "get-url", "origin"), url)
            self.assertEqual((restored / "fixture.el").read_text(), "original")
            self.assertFalse((restored / "generated.elc").exists())
            self.assertEqual(git(restored, "status", "--porcelain"), "")
            other = root / "other"
            other.mkdir()
            cache_module.restore(other, cache, {"fixture": "0" * 40})
            self.assertFalse((other / "fixture").exists())
            (cache / "fixture" / "metadata.json").write_text("invalid JSON")
            with contextlib.redirect_stdout(io.StringIO()):
                cache_module.restore(other, cache, pinned)
            self.assertFalse((other / "fixture").exists())

    def test_does_not_accept_authenticated_or_local_remote_urls(self):
        for url in (
            "/tmp/local-repo",
            "file:///tmp/local-repo",
            "https://token@example.invalid/fixture.git",
            "https://example.invalid/fixture.git?token=secret",
        ):
            self.assertFalse(cache_module.public_url(url))


if __name__ == "__main__":
    unittest.main()
