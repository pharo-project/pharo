initialization
wrapBlock: aFourArgBlock

	^[ :deltaAddress :hunkAddress :lineAddress :payload |
		aFourArgBlock 
			value: (LGitDiffDelta fromHandle: deltaAddress) 
			value: (LGitDiffHunk fromHandle: hunkAddress)
			value: (LGitDiffLine fromHandle: lineAddress)
			value: payload ]