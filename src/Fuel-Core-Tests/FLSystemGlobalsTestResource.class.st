"
I create a deep copy of the system globals to prevent their pollution. When I am cleaned up, I simply forget the copy and the system globals will remain in their original state.

Note that copying the system globals is very expensive, so only use me where necessary.
"
Class {
	#name : 'FLSystemGlobalsTestResource',
	#superclass : 'TestResource',
	#instVars : [
		'globals'
	],
	#category : 'Fuel-Core-Tests-Support-Resources',
	#package : 'Fuel-Core-Tests',
	#tag : 'Support-Resources'
}

{ #category : 'accessing' }
FLSystemGlobalsTestResource >> globals [
	^ globals
]

{ #category : 'running' }
FLSystemGlobalsTestResource >> setUp [
	super setUp.

	globals := SystemEnvironment new: Smalltalk globals size.
	"Can't use #newFrom: because associations must be unique"
	Smalltalk globals keysAndValuesDo: [ :key :value |
		globals
			at: key
			put: value ]
]

{ #category : 'running' }
FLSystemGlobalsTestResource >> tearDown [
	globals := nil.

	super tearDown
]
