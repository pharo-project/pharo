Class {
	#name : #LGitPackbuilderForeachCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitPackbuilderForeachCallback class >> fnSpec [
	^#(int (void *buffer, int length, void *payload))
]

{ #category : #initialization }
LGitPackbuilderForeachCallback >> signature: signature "<String>" block: aBlock [ "<BlockClosure>"
	super signature: signature block: (self wrapBlock: aBlock)
]

{ #category : #initialization }
LGitPackbuilderForeachCallback >> wrapBlock: aBlock [

	^[ :buffer :length :payload |
		aBlock 
			value: buffer 
			value: length
			value: payload ]
]
