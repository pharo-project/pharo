operations
createBranch: aString force: aBoolean
	<todo: 'this method belongs somewhere else and should allow to specify the target'>
	| reference |
	reference := LGitReference of: self.
	reference createSymbolic: 'refs/heads/' , aString target: 'refs/dummy' force: aBoolean.
	^ reference