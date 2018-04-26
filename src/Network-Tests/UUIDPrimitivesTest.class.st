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
	self assert: a > b.
	
]

{ #category : #tests }
UUIDPrimitivesTest >> testCreation [
	| uuid |
	uuid := UUID new.
	self should: [uuid size = 16].
	self shouldnt: [uuid isNilUUID].
	self should: [uuid asString size = 36].

]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationEquality [
	| uuid1 uuid2 |
	uuid1 := UUID new.
	uuid2 := UUID new.
	self should: [uuid1 = uuid1].
	self should: [uuid2 = uuid2].
	self shouldnt: [uuid1 = uuid2].
	self shouldnt: [uuid1 hash = uuid2 hash].

]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromString [
	| uuid string |
	string := UUID nilUUID asString.
	uuid := UUID fromString: string.
	self should: [uuid size = 16].
	self should: [uuid = UUID nilUUID].
	self should: [uuid isNilUUID].
	self should: [uuid asString size = 36].
	self should: [uuid asArray asSet size = 1].
	self should: [(uuid asArray asSet asArray at: 1) = 0].

]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromString36NotNil [
	| uuid uuid36 |
	uuid := UUID fromString: 'fedcba98-7654-3210-0123-456789abcdef'.
	uuid36 := UUID fromString36: 'e738i8a5the1f87mrh14o6vi6'.
	self should: [uuid = uuid36]
]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromString36With0 [
	| uuid |
	uuid := UUID fromString36: '0'.
	self assert: (uuid isNilUUID).
	

]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromString36WithNillUUID [
	
	UUID fromString36: (UUID nilUUID asString36).
]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationFromStringNotNil [
	| uuid string |
	string := UUID new asString.
	uuid := UUID fromString: string.
	self should: [uuid size = 16].
	self should: [uuid asString size = 36].


]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationNil [
	| uuid |
	uuid := UUID nilUUID.
	self should: [uuid size = 16].
	self should: [uuid isNilUUID].
	self should: [uuid asString size = 36].
	self should: [uuid asArray asSet size = 1].
	self should: [(uuid asArray asSet asArray at: 1) = 0].

]

{ #category : #tests }
UUIDPrimitivesTest >> testCreationNodeBased [
	

	(UUID new asString last: 12) = (UUID new asString last: 12) ifFalse: [^self].
	1000 timesRepeat:
		[ | uuid |uuid := UUID new.
		self should: [((uuid at: 7) bitAnd: 16rF0) = 16r10].
		self should: [((uuid at: 9) bitAnd: 16rC0) = 16r80]]

]

{ #category : #tests }
UUIDPrimitivesTest >> testDuplicationsKinda [
	| check size |

	size := 5000.
	check := Set new: size.
	size timesRepeat: 
		[ | uuid |uuid := UUID new.
		self shouldnt: [check includes: uuid].
		check add: uuid].
		
]

{ #category : #tests }
UUIDPrimitivesTest >> testOrder [
	
	100 timesRepeat:
		[ | uuid1 uuid2 |uuid1 := UUID new.
		uuid2 := UUID new.
		(uuid1 asString last: 12) = (uuid2 asString last: 12) ifTrue:
			[self should: [uuid1 < uuid2].
			self should: [uuid2 > uuid1].
			self shouldnt: [uuid1 = uuid2]]]

]

{ #category : #tests }
UUIDPrimitivesTest >> testUniqueness [ 
	"Test uniqueness for a given number of generated values"
	
	|maxTestValue|
	maxTestValue := 1000.
	self assert: ((1 to: maxTestValue) collect: [ :i | UUID new asString ]) asSet size = maxTestValue
]
