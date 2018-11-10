"
/**
 * Signature of a function which acquires a credential object.
 *
 * - cred: The newly created credential object.
 * - url: The resource for which we are demanding a credential.
 * - username_from_url: The username that was embedded in a ""user@host""
 *                          remote url, or NULL if not included.
 * - allowed_types: A bitmask stating which cred types are OK to return.
 * - payload: The payload provided when specifying this callback.
 * - returns 0 for success, < 0 to indicate an error, > 0 to indicate
 *       no credential was acquired
 */
"
Class {
	#name : #LGitCredAcquireCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitCredAcquireCallback class >> fnSpec [
	^#(int (LGitCredentials **cred, String url, String username_from_url, uint allowed_types, void *data))
]

{ #category : #'as yet unclassified' }
LGitCredAcquireCallback >> signature: signature "<String>" block: aBlock [ "<BlockClosure>"
	super signature: signature block: (self wrapBlock: aBlock)
]

{ #category : #initialization }
LGitCredAcquireCallback >> wrapBlock: aBlock [

	^[ :credAddress :url :username_from_url :allowed_types :data |
		aBlock valueWithArguments: (Array
			with: (LGitCredentials fromHandle: credAddress) 
			with: url
			with: username_from_url
			with: allowed_types 
			with: data) ]
]
