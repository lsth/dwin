# development notes
## 1. pre-commit checks
install once:
```bash
	cargo install lychee
	pipx install pre-commit
	
	# in the project directory:
	pre-commit install
	
    # in etc/
	make dev-emacs
```

to check manually before, say in the project directory:
```bash
	pre-commit run --all-files
```

for further checks, you can say in `etc/`
```bash
	make check-melpazoid
```
(if [melpazoid](https://github.com/riscy/melpazoid) has been installed in `~/local/system/misc/melpazoid`).

## 2. markdown preview
install once:
```bash
go install github.com/chrishrb/go-grip@latest
```

to preview, in project directory:
```bash
    go-grip -p 8100 README.md
```

## 3. releases and versions
1. update `dwin.el`  header 
   and create a commit "bump version to 0.1"
   (as a stand-alone commit)
2. create a git tag:
```
git tag -a v0.1 -m "Release version 0.1"
git push origin main --tags
```
