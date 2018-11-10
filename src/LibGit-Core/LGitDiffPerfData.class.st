Class {
	#name : #LGitDiffPerfData,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_OID_CALCULATIONS',
		'OFFSET_PRIM_STAT_CALLS',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffPerfData class >> fieldsDesc [
	^#(
	uint version
	size_t stat_calls "Number of stat() calls performed"
	size_t oid_calculations "Number of ID calculations"
	)
]

{ #category : #'libgit-fields' }
LGitDiffPerfData >> prim_oid_calculations [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_OID_CALCULATIONS
]

{ #category : #'libgit-fields' }
LGitDiffPerfData >> prim_oid_calculations: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_OID_CALCULATIONS put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffPerfData >> prim_stat_calls [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_STAT_CALLS
]

{ #category : #'libgit-fields' }
LGitDiffPerfData >> prim_stat_calls: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_STAT_CALLS put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffPerfData >> prim_version [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_VERSION
]

{ #category : #'libgit-fields' }
LGitDiffPerfData >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject
]
