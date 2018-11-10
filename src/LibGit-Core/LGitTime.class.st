Class {
	#name : #LGitTime,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_OFFSET',
		'OFFSET_PRIM_TIME'
	],
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #'fields description' }
LGitTime class >> fieldsDesc [
	^#(
	int64 time;
	int offset;
	)
]

{ #category : #'instance creation' }
LGitTime class >> fromDateAndTime: aDateAndTime [
	^ self new 
		time: aDateAndTime asUnixTime;
		offset: aDateAndTime offset asMinutes asInteger;
		yourself
]

{ #category : #comparing }
LGitTime >> = anObject [
	^ self == anObject
		or: [ self class == anObject class
			and: [ self time = anObject time
				and: [  self offset = anObject offset ] ] ]
]

{ #category : #converting }
LGitTime >> asDateAndTime [
	^ (DateAndTime fromUnixTime: self time) 
		offset: self offset;
		yourself
]

{ #category : #comparing }
LGitTime >> hash [
	^ (super hash bitXor: self time) bitXor: self offset
]

{ #category : #printing }
LGitTime >> objectPrintOn: aStream [
	self asDateAndTime printOn: aStream
]

{ #category : #accessing }
LGitTime >> offset [
	^ self prim_offset
]

{ #category : #accessing }
LGitTime >> offset: anInteger [
	self prim_offset: anInteger
]

{ #category : #'libgit-fields' }
LGitTime >> prim_offset [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_OFFSET
]

{ #category : #'libgit-fields' }
LGitTime >> prim_offset: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_OFFSET put: anObject
]

{ #category : #'libgit-fields' }
LGitTime >> prim_time [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_PRIM_TIME
]

{ #category : #'libgit-fields' }
LGitTime >> prim_time: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_PRIM_TIME put: anObject
]

{ #category : #accessing }
LGitTime >> time [
	^ self prim_time
]

{ #category : #accessing }
LGitTime >> time: anInteger [
	^ self prim_time: anInteger
]
