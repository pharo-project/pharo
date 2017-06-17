enum declaration
enumDecl
	^#(
	GIT_SUBMODULE_IGNORE_RESET     -1 "reset to on-disk value"

	GIT_SUBMODULE_IGNORE_NONE      1  "any change or untracked == dirty"
	GIT_SUBMODULE_IGNORE_UNTRACKED 2  "dirty if tracked files change"
	GIT_SUBMODULE_IGNORE_DIRTY     3  "only dirty if HEAD moved"
	GIT_SUBMODULE_IGNORE_ALL       4  "never dirty"

	GIT_SUBMODULE_IGNORE_DEFAULT   0
)