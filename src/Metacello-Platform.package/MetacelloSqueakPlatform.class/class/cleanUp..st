initialize-release
cleanUp: aggressive

	Smalltalk globals at: #MCGitHubRepository ifPresent: [:c |
		c flushETagsCache].