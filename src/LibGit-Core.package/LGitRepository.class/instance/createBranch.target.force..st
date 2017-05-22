operations
createBranch: aString target: aGitReference force: aBoolean
	<todo: 'this method belongs somewhere else'>
	| reference |
	reference := LGitReference of: self.
	reference 
		createDirect: 'refs/heads/' , aString 
		target: aGitReference object id 
		force: aBoolean.
	^ reference