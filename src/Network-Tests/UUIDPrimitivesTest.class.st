"
SUnit tests for UUIDPrimitives 
"
Class {
	#name : #UUIDPrimitivesTest,
	#superclass : #TestCase,
	#category : #'Network-Tests-UUID'
}

{ #category : #tests }
UUIDPrimitivesTest >> testComparison [
	
	|a b|
	a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
	self assert: a < b.
	self deny: a> b.
	
	a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df642'.
	self assert: a < b.
	
	a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df640'.
	self assert: a > b
]

{ #category : #tests }
UUIDPrimitivesTest >> testCreation [
	| uuid |
	uuid := UUID new.
	self assert: uuid size equals: 16.
	self deny: uuid isNilUUID.
	self assert: uuid asString size equals: 36

]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationEquality [
	| uuid1 uuid2 |
	uuid1 := UUID new.
	uuid2 := UUID new.
	self assert: uuid1 equals: uuid1.
	self assert: uuid2 equals: uuid2.
	self deny: uuid1 equals: uuid2.
	self deny: uuid1 hash equals: uuid2 hash
]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromString [
	| uuid string |
	string := UUID nilUUID asString.
	uuid := UUID fromString: string.
	self assert: uuid size equals: 16.
	self assert: uuid equals: UUID nilUUID.
	self assert: uuid isNilUUID.
	self assert: uuid asString size equals: 36.
	self assert: uuid asArray asSet size equals: 1.
	self assert: (uuid asArray asSet asArray at: 1) equals: 0

]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromString36NotNil [
	| uuid uuid36 |
	uuid := UUID fromString: 'fedcba98-7654-3210-0123-456789abcdef'.
	uuid36 := UUID fromString36: 'e738i8a5the1f87mrh14o6vi6'.
	self assert: uuid equals: uuid36
]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromString36With0 [
	| uuid |
	uuid := UUID fromString36: '0'.
	self assert: uuid isNilUUID
	

]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromString36WithNillUUID [
	
	UUID fromString36: UUID nilUUID asString36
]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromStringNotNil [
	| uuid string |
	string := UUID new asString.
	uuid := UUID fromString: string.
	self assert: uuid size equals: 16.
	self assert: uuid asString size equals: 36
]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationNil [
	| uuid |
	uuid := UUID nilUUID.
	self assert: uuid size equals: 16.
	self assert: uuid isNilUUID.
	self assert: uuid asString size equals: 36.
	self assert: uuid asArray asSet size equals: 1.
	self assert: (uuid asArray asSet asArray at: 1) equals: 0
]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationNodeBased [
	

	(UUID new asString last: 12) = (UUID new asString last: 12) ifFalse: [^self].
	1000 timesRepeat:
		[ | uuid |uuid := UUID new.
		self assert: ((uuid at: 7) bitAnd: 16rF0) equals: 16r10.
		self assert: ((uuid at: 9) bitAnd: 16rC0) equals: 16r80 ]

]

{ #category : #tests }
UUIDPrimitivesTest >> testDuplicationsKinda [
	| check size |

	size := 5000.
	check := Set new: size.
	size timesRepeat: 
		[ | uuid |uuid := UUID new.
		self deny: (check includes: uuid).
		check add: uuid].
		
]

{ #category : #tests }
UUIDPrimitivesTest >> testOrder [
	100
		timesRepeat: [ | uuid1 uuid2 |
			uuid1 := UUID new.
			uuid2 := UUID new.
			(uuid1 asString last: 12) = (uuid2 asString last: 12)
				ifTrue: [ self assert: uuid1 < uuid2.
					self assert: uuid2 > uuid1.
					self deny: uuid1 equals: uuid2 ] ]
]

{ #category : #tests }
UUIDPrimitivesTest >> testUniqueness [ 
	"Test uniqueness for a given number of generated values"
	
	|maxTestValue|
	maxTestValue := 1000.
	self assert: ((1 to: maxTestValue) collect: [ :i | UUID new asString ]) asSet size equals: maxTestValue
]
