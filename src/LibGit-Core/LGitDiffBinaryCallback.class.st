Class {
	#name : #LGitDiffBinaryCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitDiffBinaryCallback class >> fnSpec [
	^#(int (LGitDiffDelta *delta, LGitDiffBinary *binary, void *payload))
]

{ #category : #initialization }
LGitDiffBinaryCallback >> signature: signature "<String>" block: aBlock [ "<BlockClosure>"
	super signature: signature block: (self wrapBlock: aBlock)
]

{ #category : #initialization }
LGitDiffBinaryCallback >> wrapBlock: aThreeArgBlock [

	^[ :deltaAddress :binary :payload |
		aThreeArgBlock 
			value: (LGitDiffDelta fromHandle: deltaAddress) 
			value: (LGitDiffBinary fromHandle: binary) 
			value: payload ]
]
