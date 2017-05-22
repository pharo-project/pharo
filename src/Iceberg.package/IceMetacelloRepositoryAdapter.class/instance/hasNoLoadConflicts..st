testing
hasNoLoadConflicts: anMCGitBasedRepository
	"Copied from MCGitBasedNetworkRepository"
  	(anMCGitBasedRepository isKindOf: self class) ifFalse: [ ^ false ].

	^ self projectPath = anMCGitBasedRepository projectPath and: [
		self repoPath = anMCGitBasedRepository repoPath and: [ 
			self projectVersion = anMCGitBasedRepository projectVersion ] ]