package arith_test

import (
	"bytes"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

var (
	testPath = func() string {
		cwd, err := os.Getwd()
		panicErr(err)
		return cwd
	}()
	projectRoot = filepath.Dir(filepath.Dir(testPath))
	testDir     = os.DirFS(testPath)
	inOut       = func() map[string]string {
		m := make(map[string]string)
		panicErr(fs.WalkDir(testDir, ".", func(path string, d fs.DirEntry, err error) error {
			parts := strings.Split(path, ".")
			if len(parts) == 3 && parts[1] == "in" {
				m[filepath.Join(testPath, path)] = strings.Join([]string{parts[0], "out.txt"}, ".")
			}
			return err
		}))
		return m
	}()
)

func panicErr(err error) {
	if err != nil {
		panic(err)
	}
}

func test(name string, args ...string) func(t *testing.T) {
	return func(t *testing.T) {
		for in, out := range inOut {
			got, err := exec.Command(name, append(args, in)...).CombinedOutput()
			if _, ok := err.(*exec.ExitError); !ok && err != nil {
				t.Fatal(err)
			}
			want, err := fs.ReadFile(testDir, out)
			if err != nil {
				t.Fatal(err)
			}
			if bytes.Compare(got, want) != 0 {
				t.Errorf("%s does not match output:\n`%s`", out, got)
			}
		}
	}
}

func run(name string, args ...string) error {
	cmd := exec.Command(name, args...)
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func TestGo(t *testing.T) {
	goDir := filepath.Join(projectRoot, "go", "arith")
	os.Chdir(goDir)
	if err := run("go", "build"); err != nil {
		t.Fatal(err)
	}
	t.Run("SmallStep", test("./arith", "-small-step"))
	t.Run("BigStep", test("./arith", "-big-step"))
}

func TestSML(t *testing.T) {
	smlDir := filepath.Join(projectRoot, "sml", "arith")
	os.Chdir(smlDir)
	if err := run(
		"mlton",
		"-default-ann", "allowExtendedTextConsts true",
		"-default-ann", "allowOrPats true",
		"arith.sml",
	); err != nil {
		t.Fatal(err)
	}
	t.Run("SmallStep", test("./arith", "-small-step"))
	t.Run("BigStep", test("./arith", "-big-step"))
}

func TestC(t *testing.T) {
	cDir := filepath.Join(projectRoot, "c", "arith")
	os.Chdir(cDir)
	if err := run("cc", "-o", "arith", "arith.c"); err != nil {
		t.Fatal(err)
	}
	t.Run("SmallStep", test("./arith", "-small-step"))
	t.Run("BigStep", test("./arith", "-big-step"))
}

func TestRust(t *testing.T) {
	rustDir := filepath.Join(projectRoot, "rust")
	os.Chdir(rustDir)
	if err := run("cargo", "build", "--quiet", "--release", "-p", "arith"); err != nil {
		t.Fatal(err)
	}
	t.Run("SmallStep", test("./target/release/arith", "-small-step"))
	t.Run("BigStep", test("./target/release/arith", "-big-step"))
}

func TestRaku(t *testing.T) {
	rakuDir := filepath.Join(projectRoot, "raku", "arith")
	os.Chdir(rakuDir)
	t.Run("SmallStep", test("raku", "arith.raku", "-small-step"))
	t.Run("BigStep", test("raku", "arith.raku", "-big-step"))
}
