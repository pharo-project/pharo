Class {
	#name : #LGitReferenceTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitReferenceTypeEnum class >> enumDecl [
	^#(
	GIT_REF_INVALID 0
	GIT_REF_OID 1
	GIT_REF_SYMBOLIC 2
	GIT_REF_LISTALL 3
)
]

{ #category : #'accessing-values' }
LGitReferenceTypeEnum class >> git_ref_invalid [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitReferenceTypeEnum class >> git_ref_listall [
	^ self basicNew
		value: 3;
		yourself
]

{ #category : #'accessing-values' }
LGitReferenceTypeEnum class >> git_ref_oid [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitReferenceTypeEnum class >> git_ref_symbolic [
	^ self basicNew
		value: 2;
		yourself
]
