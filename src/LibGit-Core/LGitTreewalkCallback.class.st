Class {
	#name : #LGitTreewalkCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitTreewalkCallback class >> fnSpec [
	^#(int (String name, LGitTreeEntry entry, void *payload))
]

{ #category : #initialization }
LGitTreewalkCallback >> signature: signature "<String>" block: aBlock [ "<BlockClosure>"
	super signature: signature block: (self wrapBlock: aBlock)
]

{ #category : #initialization }
LGitTreewalkCallback >> wrapBlock: aThreeArgBlock [

	^[ :rootPath :entryAddress :payload |
		aThreeArgBlock 
			value: rootPath 
			value: (LGitTreeEntry fromHandle: entryAddress)
			value: payload ]
		
]
