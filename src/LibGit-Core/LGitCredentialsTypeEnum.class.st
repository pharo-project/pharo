Class {
	#name : #LGitCredentialsTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitCredentialsTypeEnum class >> enumDecl [
	^#(
	GIT_CREDTYPE_USERPASS_PLAINTEXT 1
	GIT_CREDTYPE_SSH_KEY 2
	GIT_CREDTYPE_SSH_CUSTOM 4
	GIT_CREDTYPE_DEFAULT 8
)
]

{ #category : #'accessing-values' }
LGitCredentialsTypeEnum class >> git_credtype_default [
	^ self basicNew
		value: 8;
		yourself
]

{ #category : #'accessing-values' }
LGitCredentialsTypeEnum class >> git_credtype_ssh_custom [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitCredentialsTypeEnum class >> git_credtype_ssh_key [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitCredentialsTypeEnum class >> git_credtype_userpass_plaintext [
	^ self basicNew
		value: 1;
		yourself
]
