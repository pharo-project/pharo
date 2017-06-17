operations
createTag: aString targetId: aGitId force: aBoolean
	"Create a tag. Based on #createBranch:targetId:force:"

	| reference |
	reference := LGitReference of: self.
	reference
		createDirect: 'refs/tags/', aString
		target: aGitId
		force: aBoolean.
	^ reference