initialization
wrapBlock: aThreeArgBlock

	^[ :rootPath :entryAddress :payload |
		aThreeArgBlock 
			value: rootPath 
			value: (LGitTreeEntry fromHandle: entryAddress)
			value: payload ]
		