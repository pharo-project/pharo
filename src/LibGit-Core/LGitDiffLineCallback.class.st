Class {
	#name : #LGitDiffLineCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitDiffLineCallback class >> fnSpec [
	^#(int (LGitDiffDelta *delta, LGitDiffHunk *hunk, LGitDiffLine *line, void *payload))
]

{ #category : #initialization }
LGitDiffLineCallback >> signature: signature "<String>" block: aBlock [ "<BlockClosure>"
	super signature: signature block: (self wrapBlock: aBlock)
]

{ #category : #initialization }
LGitDiffLineCallback >> wrapBlock: aFourArgBlock [

	^[ :deltaAddress :hunkAddress :lineAddress :payload |
		aFourArgBlock 
			value: (LGitDiffDelta fromHandle: deltaAddress) 
			value: (LGitDiffHunk fromHandle: hunkAddress)
			value: (LGitDiffLine fromHandle: lineAddress)
			value: payload ]
]
