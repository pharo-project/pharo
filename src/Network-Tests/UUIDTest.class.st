"
SUnit tests for UUID
"
Class {
	#name : #UUIDTest,
	#superclass : #TestCase,
	#category : #'Network-Tests-UUID'
}

{ #category : #testing }
UUIDTest >> testBasic [
	| uuid |
	uuid := UUID new.
	self assert: uuid size equals: 16.
	self deny: uuid isNilUUID.
	self deny: uuid = UUID new.
]

{ #category : #testing }
UUIDTest >> testComparison [
	|a b|
	
	a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
	
	self assert: a = a copy.
	self assert: a <= a copy.
	self assert: a >= a copy.
	self assert: b = b copy.
	self assert: b <= b copy.
	self assert: b >= b copy.
	self assert: a < b.
	self assert: a <= b.
	self assert: b > a.
	self assert: b >= a.
	
	self deny: (a > b) = (b > a).
	self deny: (a >= b) = (b >= a).
	
	
]

{ #category : #testing }
UUIDTest >> testNilUUID [
	| nilUUIDRepresentation |
	nilUUIDRepresentation := '00000000-0000-0000-0000-000000000000'.
	self assert: UUID nilUUID isNilUUID.
	self assert: UUID nilUUID asString equals: nilUUIDRepresentation.
	self assert: (UUID fromString: nilUUIDRepresentation) equals: UUID nilUUID.
	self deny: UUID new isNilUUID 
]

{ #category : #testing }
UUIDTest >> testString [

	| s1 s2 s3 |
	
	s1 := '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	s2 := 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
	s3 := '00000000-0000-0000-0000-000000000000'.
	
	self assert: (UUID fromString: s1) printString equals: s1.
	self assert: (UUID fromString: s2) printString equals: s2.
	self assert: UUID nilUUID printString equals: s3.	

	self assert: (UUID fromString: s1) asString equals: s1.
	self assert: (UUID fromString: s2) asString equals: s2.
	self assert: UUID nilUUID asString equals: s3.	

]

{ #category : #testing }
UUIDTest >> testString36 [
	| uuid |
	uuid := UUID new.
	self assert: (uuid asString36 allSatisfy: [ :e | e isAlphaNumeric ]).
	self assert: (UUID fromString36: uuid asString36) equals: uuid.

	self assert: UUID nilUUID asString36 equals: 0 asString.
	self assert: (UUID fromString36: 0 asString) equals: UUID nilUUID 
]
