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

