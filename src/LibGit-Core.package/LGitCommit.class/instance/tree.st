accessing
tree
	| tree |
	tree := LGitTree of: self repository.
	self commit_tree: tree commit: self.
	^ tree