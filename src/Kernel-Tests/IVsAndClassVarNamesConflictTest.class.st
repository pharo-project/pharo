"
SUnit tests to tests conflicts for instance variables and class variable names
"
Class {
	#name : 'IVsAndClassVarNamesConflictTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'running' }
IVsAndClassVarNamesConflictTest >> setUp [
	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
IVsAndClassVarNamesConflictTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
IVsAndClassVarNamesConflictTest >> testOneCanProceedWhenIntroducingCapitalizedInstanceVariables [

	| class |
	[ class := classFactory make: [ :aBuilder | aBuilder slotsFromString: 'X Y' ] ]
		on: Exception
		do: [ :ex | ex resume ].
	self assert: (testingEnvironment keys includes: class name)
]

{ #category : 'tests' }
IVsAndClassVarNamesConflictTest >> testOneCanProceedWhenIntroducingClasseVariablesBeginingWithLowerCaseCharacters [

	| class |
	[ class := classFactory make: [ :aBuilder | aBuilder slotsFromString: 'a BVarName' ] ]
		on: Exception
		do: [ :ex | ex resume ].
	self assert: (testingEnvironment keys includes: class name)
]
