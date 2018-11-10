"
I'm an unique identity of any object (commit, tree, blob, tag).

https://libgit2.github.com/libgit2/#HEAD/type/git_oid

NOTE: This might be replaces by an FFITypeArray in the future.
"
Class {
	#name : #LGitId,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'Byte20',
		'OFFSET_PRIM_DATA1',
		'OFFSET_PRIM_DATA10',
		'OFFSET_PRIM_DATA11',
		'OFFSET_PRIM_DATA12',
		'OFFSET_PRIM_DATA13',
		'OFFSET_PRIM_DATA14',
		'OFFSET_PRIM_DATA15',
		'OFFSET_PRIM_DATA16',
		'OFFSET_PRIM_DATA17',
		'OFFSET_PRIM_DATA18',
		'OFFSET_PRIM_DATA19',
		'OFFSET_PRIM_DATA2',
		'OFFSET_PRIM_DATA20',
		'OFFSET_PRIM_DATA3',
		'OFFSET_PRIM_DATA4',
		'OFFSET_PRIM_DATA5',
		'OFFSET_PRIM_DATA6',
		'OFFSET_PRIM_DATA7',
		'OFFSET_PRIM_DATA8',
		'OFFSET_PRIM_DATA9'
	],
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #accessing }
LGitId class >> fieldsDesc [
	^#(
	uint8 data1;
	uint8 data2;
	uint8 data3;
	uint8 data4;
	uint8 data5;
	uint8 data6;
	uint8 data7;
	uint8 data8;
	uint8 data9;
	uint8 data10;
	uint8 data11;
	uint8 data12;
	uint8 data13;
	uint8 data14;
	uint8 data15;
	uint8 data16;
	uint8 data17;
	uint8 data18;
	uint8 data19;
	uint8 data20;
	)
]

{ #category : #'instance creation' }
LGitId class >> fromByteArray: aByteArray [
	^ self new
		prim_data1: (aByteArray at: 1);
		prim_data2: (aByteArray at: 2);
		prim_data3: (aByteArray at: 3);
		prim_data4: (aByteArray at: 4);
		prim_data5: (aByteArray at: 5);
		prim_data6: (aByteArray at: 6);
		prim_data7: (aByteArray at: 7);
		prim_data8: (aByteArray at: 8);
		prim_data9: (aByteArray at: 9);
		prim_data10: (aByteArray at: 10);
		prim_data11: (aByteArray at: 11);
		prim_data12: (aByteArray at: 12);
		prim_data13: (aByteArray at: 13);
		prim_data14: (aByteArray at: 14);
		prim_data15: (aByteArray at: 15);
		prim_data16: (aByteArray at: 16);
		prim_data17: (aByteArray at: 17);
		prim_data18: (aByteArray at: 18);
		prim_data19: (aByteArray at: 19);
		prim_data20: (aByteArray at: 20);
		yourself
]

{ #category : #'instance creation' }
LGitId class >> fromHexString: aString [
	^ self fromByteArray: (ByteArray readHexFrom: aString)
]

{ #category : #comparing }
LGitId >> = anObject [
	^ self == anObject
		or: [ self class = anObject class
			and: [ self compare: anObject ] ]
]

{ #category : #converting }
LGitId >> asByteArray [
	^ (ByteArray new: 20)
		at: 1 put: (self prim_data1);
		at: 2 put: (self prim_data2);
		at: 3 put: (self prim_data3);
		at: 4 put: (self prim_data4);
		at: 5 put: (self prim_data5);
		at: 6 put: (self prim_data6);
		at: 7 put: (self prim_data7);
		at: 8 put: (self prim_data8);
		at: 9 put: (self prim_data9);
		at: 10 put: (self prim_data10);
		at: 11 put: (self prim_data11);
		at: 12 put: (self prim_data12);
		at: 13 put: (self prim_data13);
		at: 14 put: (self prim_data14);
		at: 15 put: (self prim_data15);
		at: 16 put: (self prim_data16);
		at: 17 put: (self prim_data17);
		at: 18 put: (self prim_data18);
		at: 19 put: (self prim_data19);
		at: 20 put: (self prim_data20);
		yourself
	
]

{ #category : #converting }
LGitId >> asInteger [
	^ self asByteArray asInteger
]

{ #category : #printing }
LGitId >> asRelativePathString [
	| string |
	string := String new: 41.
	string pin.
	self oid_pathfmt: string id: self.
	string unpin.
	^ string
]

{ #category : #private }
LGitId >> compare: anId [
	^ (self 
		oid_cmp: self
		b: anId) isZero
]

{ #category : #printing }
LGitId >> hexString [
	| string |
	self isExternal
		ifFalse: [ ^ handle hex ].
	string := String new: 40.
	string pin.
	self oid_fmt: string id: self.
	string unpin.
	^ string
]

{ #category : #private }
LGitId >> internalize [
	"Copy the data of this object over to Smalltalk memory if it is currently external.
	Now we can keep on using this object after the containing object (Blob, Tree or Commit)
	has been freed."

	(handle isExternalAddress and: [ handle isNull not ])
		ifTrue: [ 
			| bytes |
			bytes := handle copyFrom: 1 to: self class byteSize.
			handle := bytes ]
]

{ #category : #printing }
LGitId >> objectPrintOn: aStream [
	aStream nextPutAll: self hexString surroundedBySingleQuotes
]

{ #category : #'libgit-calls' }
LGitId >> oid_cmp: a b: b [ 
	^self call: #(int git_oid_cmp(LGitId * self, LGitId * b)) options: #(  )
]

{ #category : #'libgit-calls' }
LGitId >> oid_fmt: out id:  objectId [
	^self call: #(void git_oid_fmt(void *out, LGitId * self)) options: #(  )
]

{ #category : #'libgit-calls' }
LGitId >> oid_pathfmt: out id: id [ 
	^ self call: #(void git_oid_pathfmt(void *out, LGitId * self)) options: #(  )
]

{ #category : #'libgit-fields' }
LGitId >> prim_data1 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA1
]

{ #category : #'libgit-fields' }
LGitId >> prim_data10 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA10
]

{ #category : #'libgit-fields' }
LGitId >> prim_data10: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA10 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data11 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA11
]

{ #category : #'libgit-fields' }
LGitId >> prim_data11: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA11 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data12 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA12
]

{ #category : #'libgit-fields' }
LGitId >> prim_data12: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA12 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data13 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA13
]

{ #category : #'libgit-fields' }
LGitId >> prim_data13: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA13 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data14 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA14
]

{ #category : #'libgit-fields' }
LGitId >> prim_data14: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA14 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data15 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA15
]

{ #category : #'libgit-fields' }
LGitId >> prim_data15: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA15 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data16 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA16
]

{ #category : #'libgit-fields' }
LGitId >> prim_data16: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA16 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data17 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA17
]

{ #category : #'libgit-fields' }
LGitId >> prim_data17: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA17 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data18 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA18
]

{ #category : #'libgit-fields' }
LGitId >> prim_data18: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA18 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data19 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA19
]

{ #category : #'libgit-fields' }
LGitId >> prim_data19: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA19 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data1: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA1 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data2 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA2
]

{ #category : #'libgit-fields' }
LGitId >> prim_data20 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA20
]

{ #category : #'libgit-fields' }
LGitId >> prim_data20: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA20 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data2: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA2 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data3 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA3
]

{ #category : #'libgit-fields' }
LGitId >> prim_data3: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA3 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data4 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA4
]

{ #category : #'libgit-fields' }
LGitId >> prim_data4: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA4 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data5 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA5
]

{ #category : #'libgit-fields' }
LGitId >> prim_data5: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA5 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data6 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA6
]

{ #category : #'libgit-fields' }
LGitId >> prim_data6: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA6 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data7 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA7
]

{ #category : #'libgit-fields' }
LGitId >> prim_data7: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA7 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data8 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA8
]

{ #category : #'libgit-fields' }
LGitId >> prim_data8: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA8 put: anObject
]

{ #category : #'libgit-fields' }
LGitId >> prim_data9 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PRIM_DATA9
]

{ #category : #'libgit-fields' }
LGitId >> prim_data9: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PRIM_DATA9 put: anObject
]
