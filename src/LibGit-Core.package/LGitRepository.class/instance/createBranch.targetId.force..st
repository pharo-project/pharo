operations
createBranch: aString targetId: aGitId force: aBoolean
	<todo: 'this method belongs somewhere else'>
	| reference |
	reference := LGitReference of: self.
	reference createDirect: 'refs/heads/' , aString target: aGitId force: aBoolean.
	^ reference