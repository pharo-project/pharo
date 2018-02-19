"
SUnit tests to tests conflicts for instance variables and class variable names
"
Class {
	#name : #IVsAndClassVarNamesConflictTest,
	#superclass : #TestCase,
	#instVars : [
		'classFactory'
	],
	#category : #'Kernel-Tests-Classes'
}

{ #category : #running }
IVsAndClassVarNamesConflictTest >> setUp [
	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : #running }
IVsAndClassVarNamesConflictTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : #tests }
IVsAndClassVarNamesConflictTest >> testOneCanProceedWhenIntroducingCapitalizedInstanceVariables [
	| class |
	[ 
		class := classFactory
			newSubclassOf: classFactory defaultSuperclass
			instanceVariableNames: 'X Y'
			classVariableNames: ''
	]
		on: Exception
		do: [ :ex | ex resume ].
	self assert: (Smalltalk keys includes: class name)
]

{ #category : #tests }
IVsAndClassVarNamesConflictTest >> testOneCanProceedWhenIntroducingClasseVariablesBeginingWithLowerCaseCharacters [
	| class |
	[ 
		class := classFactory
			newSubclassOf: classFactory defaultSuperclass
			instanceVariableNames: 'a BVarName'
			classVariableNames: ''
	]
		on: Exception
		do: [ :ex | ex resume ].
	self assert: (Smalltalk keys includes: class name)
]
