"""Build a separate test-only virtualenv with a named Jupyter kernelspec.

Reuse the runner's installed dependencies via a .pth file, without downloads.
The fixture module exists only in the new environment's own site-packages.
"""
import pathlib
import site
import subprocess
import sys
import venv


def main():
    root = pathlib.Path(sys.argv[1]).resolve()
    venv.EnvBuilder(with_pip=False).create(root)
    python = root / "bin" / "python"
    packages = pathlib.Path(
        subprocess.check_output(
            [str(python), "-c", "import sysconfig; print(sysconfig.get_path('purelib'))"],
            text=True,
        ).strip()
    )
    dependencies = [p for p in site.getsitepackages() if pathlib.Path(p).is_dir()]
    (packages / "rvf-test-dependencies.pth").write_text("\n".join(dependencies) + "\n")
    (packages / "rvf_kernel_fixture.py").write_text("VALUE = 12345\n")
    subprocess.run(
        [
            str(python),
            "-m",
            "ipykernel",
            "install",
            "--user",
            "--name",
            "rvf-dedicated",
            "--display-name",
            "Dedicated regression kernel",
        ],
        check=True,
    )


if __name__ == "__main__":
    main()
