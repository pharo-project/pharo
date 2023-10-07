"
SUnit tests for serialization with ignored variables
"
Class {
	#name : 'FLIgnoredVariablesTest',
	#superclass : 'FLClassSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
FLIgnoredVariablesTest >> testAllVariablesIgnored [

	| anObject result aClass |
	aClass := self classFactory make: [ :aBuilder | aBuilder slotsFromString: 'a b' ].
	self classFactory silentlyCompile: 'fuelIgnoredInstanceVariableNames ^#(a b)' in: aClass class.
	anObject := aClass new
		            instVarAt: 1 put: $A;
		            instVarAt: 2 put: $B;
		            yourself.

	result := self resultOfSerializeAndMaterialize: anObject.

	self assert: (result instVarAt: 1) isNil.
	self assert: (result instVarAt: 2) isNil
]

{ #category : 'tests' }
FLIgnoredVariablesTest >> testIgnoredValueIsNotMaterialized [

	| anObject materialized aClass |
	aClass := self classFactory make: [ :aBuilder | aBuilder slotsFromString: 'a' ].
	self classFactory silentlyCompile: 'fuelIgnoredInstanceVariableNames ^#(a)' in: aClass class.
	anObject := aClass new
		            instVarAt: 1 put: #A;
		            yourself.

	self serialize: anObject.
	materialized := self materialized.

	self deny: (materialized instVarAt: 1) identicalTo: #A
]

{ #category : 'tests' }
FLIgnoredVariablesTest >> testOneIgnoredVariable [

	| anObject result aClass |
	aClass := self classFactory make: [ :aBuilder | aBuilder slotsFromString: 'a b c' ].
	self classFactory silentlyCompile: 'fuelIgnoredInstanceVariableNames ^#(b)' in: aClass class.
	anObject := aClass new
		            instVarAt: 1 put: $A;
		            instVarAt: 2 put: $B;
		            instVarAt: 3 put: $C;
		            yourself.

	result := self resultOfSerializeAndMaterialize: anObject.

	self assert: $A equals: (result instVarAt: 1).
	self assert: nil equals: (result instVarAt: 2).
	self assert: $C equals: (result instVarAt: 3)
]

{ #category : 'tests' }
FLIgnoredVariablesTest >> testTwoIgnoredVariables [

	| anObject result aClass |
	aClass := self classFactory make: [ :aBuilder | aBuilder slotsFromString: 'a b c' ].
	self classFactory silentlyCompile: 'fuelIgnoredInstanceVariableNames ^#(a c)' in: aClass class.
	anObject := aClass new
		            instVarAt: 1 put: $A;
		            instVarAt: 2 put: $B;
		            instVarAt: 3 put: $C;
		            yourself.

	result := self resultOfSerializeAndMaterialize: anObject.

	self assert: nil equals: (result instVarAt: 1).
	self assert: $B equals: (result instVarAt: 2).
	self assert: nil equals: (result instVarAt: 3)
]
