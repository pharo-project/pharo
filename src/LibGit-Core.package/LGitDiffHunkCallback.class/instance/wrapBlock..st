initialization
wrapBlock: aBlock

	^[ :deltaAddress :hunkAddress :payload |
		aBlock 
			value: (LGitDiffDelta fromHandle: deltaAddress) 
			value: (LGitDiffHunk fromHandle: hunkAddress)
			value: payload ]