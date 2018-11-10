Class {
	#name : #LGitDiffFileCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitDiffFileCallback class >> fnSpec [
	^#(int (LGitDiffDelta * delta, float progress, void *payload))
]

{ #category : #initialization }
LGitDiffFileCallback >> signature: signature "<String>" block: aBlock [ "<BlockClosure>"
	super signature: signature block: (self wrapBlock: aBlock)
]

{ #category : #initialization }
LGitDiffFileCallback >> wrapBlock: aThreeArgBlock [

	^[ :deltaAddress :progress :payload |
		aThreeArgBlock 
			value: (LGitDiffDelta fromHandle: deltaAddress) 
			value: progress
			value: payload ]
]
