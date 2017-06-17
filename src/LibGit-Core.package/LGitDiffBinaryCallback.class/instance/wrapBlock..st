initialization
wrapBlock: aThreeArgBlock

	^[ :deltaAddress :binary :payload |
		aThreeArgBlock 
			value: (LGitDiffDelta fromHandle: deltaAddress) 
			value: (LGitDiffBinary fromHandle: binary) 
			value: payload ]