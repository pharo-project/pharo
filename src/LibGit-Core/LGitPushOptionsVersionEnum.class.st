Class {
	#name : #LGitPushOptionsVersionEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitPushOptionsVersionEnum class >> enumDecl [
	^ #(
			LGIT_PUSH_OPTIONS_VERSION_1 1
		)
]

{ #category : #'accessing-values' }
LGitPushOptionsVersionEnum class >> lgit_push_options_version_1 [
	^ self basicNew
		value: 1;
		yourself
]
