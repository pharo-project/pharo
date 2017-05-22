initialization
wrapBlock: aThreeArgBlock

	^[ :deltaAddress :progress :payload |
		aThreeArgBlock 
			value: (LGitDiffDelta fromHandle: deltaAddress) 
			value: progress
			value: payload ]