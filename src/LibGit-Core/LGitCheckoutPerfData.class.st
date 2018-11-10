Class {
	#name : #LGitCheckoutPerfData,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_CHMOD_CALLS',
		'OFFSET_PRIM_MKDIR_CALLS',
		'OFFSET_PRIM_STAT_CALLS'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitCheckoutPerfData class >> fieldsDesc [
	^ #(
		size_t mkdir_calls;
		size_t stat_calls;
		size_t chmod_calls;
	)
]

{ #category : #'libgit-fields' }
LGitCheckoutPerfData >> prim_chmod_calls [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_CHMOD_CALLS
]

{ #category : #'libgit-fields' }
LGitCheckoutPerfData >> prim_chmod_calls: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_CHMOD_CALLS put: anObject
]

{ #category : #'libgit-fields' }
LGitCheckoutPerfData >> prim_mkdir_calls [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_MKDIR_CALLS
]

{ #category : #'libgit-fields' }
LGitCheckoutPerfData >> prim_mkdir_calls: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_MKDIR_CALLS put: anObject
]

{ #category : #'libgit-fields' }
LGitCheckoutPerfData >> prim_stat_calls [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_STAT_CALLS
]

{ #category : #'libgit-fields' }
LGitCheckoutPerfData >> prim_stat_calls: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_STAT_CALLS put: anObject
]
