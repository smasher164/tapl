package arith_test

import (
	"bytes"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

var (
	testPath = func() string {
		cwd, err := os.Getwd()
		if err != nil {
			panic(err)
		}
		return cwd
	}()
	projectRoot = filepath.Dir(filepath.Dir(testPath))
	testDir     = os.DirFS(testPath)
	inOut       = func() map[string]string {
		m := make(map[string]string)
		inputFilenames, err := fs.Glob(testDir, "*.in.txt")
		if err != nil {
			panic(err)
		}
		outputFilenames, err := fs.Glob(testDir, "*.out.txt")
		if err != nil {
			panic(err)
		}
		for i, in := range inputFilenames {
			m[filepath.Join(testPath, in)] = outputFilenames[i]
		}
		return m
	}()
)

func binExec(flag string) func(t *testing.T) {
	return func(t *testing.T) {
		for in, out := range inOut {
			got, err := exec.Command("./arith", flag, in).CombinedOutput()
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

func TestGo(t *testing.T) {
	goDir := filepath.Join(projectRoot, "go", "arith")
	os.Chdir(goDir)
	if err := exec.Command("go", "build").Run(); err != nil {
		t.Fatal(err)
	}
	t.Run("SmallStep", binExec("-small-step"))
	t.Run("BigStep", binExec("-big-step"))
}

func TestSML(t *testing.T) {
	smlDir := filepath.Join(projectRoot, "sml", "arith")
	os.Chdir(smlDir)
	if err := exec.Command(
		"mlton",
		"-default-ann", "allowExtendedTextConsts true",
		"-default-ann", "allowOrPats true",
		"arith.sml",
	).Run(); err != nil {
		t.Fatal(err)
	}
	t.Run("SmallStep", binExec("-small-step"))
	t.Run("BigStep", binExec("-big-step"))
}

func TestC(t *testing.T) {
	cDir := filepath.Join(projectRoot, "c", "arith")
	os.Chdir(cDir)
	if err := exec.Command("cc", "-o", "arith", "arith.c").Run(); err != nil {
		t.Fatal(err)
	}
	t.Run("SmallStep", binExec("-small-step"))
	t.Run("BigStep", binExec("-big-step"))
}
