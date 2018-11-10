Class {
	#name : #LGitDiffHunkCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitDiffHunkCallback class >> fnSpec [
	^#(int (LGitDiffDelta *delta, LGitDiffHunk *hunk, void *payload))
]

{ #category : #initialization }
LGitDiffHunkCallback >> signature: signature "<String>" block: aBlock [ "<BlockClosure>"
	super signature: signature block: (self wrapBlock: aBlock)
]

{ #category : #initialization }
LGitDiffHunkCallback >> wrapBlock: aBlock [

	^[ :deltaAddress :hunkAddress :payload |
		aBlock 
			value: (LGitDiffDelta fromHandle: deltaAddress) 
			value: (LGitDiffHunk fromHandle: hunkAddress)
			value: payload ]
]
