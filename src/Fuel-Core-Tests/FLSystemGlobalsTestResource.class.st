"
In order to be sure Fuel tests will not impact the global system, I copy the globals and add them back after the tests are done
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

	globals := SystemDictionary new: Smalltalk globals size.
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
