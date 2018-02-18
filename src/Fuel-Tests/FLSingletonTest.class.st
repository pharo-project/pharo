"
SUnit tests for fuel serialization of singletons
"
Class {
	#name : #FLSingletonTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core'
}

{ #category : #running }
FLSingletonTest >> setUp [
	super setUp.
	FLSingletonMock reset
]

{ #category : #tests }
FLSingletonTest >> testSingletonMaterialization [
	| singleton materialized |
	singleton := FLSingletonMock instance.
	singleton reference: 'a reference'.
	self assert: singleton equals: FLSingletonMock instance.
	
	materialized := self resultOfSerializeAndMaterialize: singleton.
	self deny: materialized == singleton.
	self deny: materialized reference isNil.
	self deny: materialized reference == singleton reference.
	
	FLSingletonMock reset "cannot do this in tearDown because that's used by some helper methods"
]

{ #category : #tests }
FLSingletonTest >> testSingletonMaterializationEnforced [
	| singleton materialized |
	singleton := FLSingletonMockEnforced instance.
	singleton reference: 'a reference'.
	self assert: singleton equals: FLSingletonMockEnforced instance.
	
	materialized := self resultOfSerializeAndMaterialize: singleton.
	self assert: materialized == singleton.
	self deny: materialized reference isNil.
	self assert: materialized reference == singleton reference.
	
	FLSingletonMock reset "cannot do this in tearDown because that's used by some helper methods"
]

{ #category : #tests }
FLSingletonTest >> testSingletonMaterializationEnforcedNoInstance [
	| singleton materialized |
	singleton := FLSingletonMockEnforced instance.
	singleton reference: 'a reference'.
	self assert: singleton equals: FLSingletonMockEnforced instance.
	
	self serialize: singleton.
	FLSingletonMock reset.
	
	materialized := self materialized.
	self deny: materialized == singleton.
	self deny: materialized reference isNil.
	self assert: materialized reference equals: singleton reference.
	
	FLSingletonMock reset "cannot do this in tearDown because that's used by some helper methods"
]
