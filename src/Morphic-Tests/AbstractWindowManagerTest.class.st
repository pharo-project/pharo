Class {
	#name : 'AbstractWindowManagerTest',
	#superclass : 'TestCase',
	#instVars : [
		'manager'
	],
	#category : 'Morphic-Tests',
	#package : 'Morphic-Tests'
}

{ #category : 'tests' }
AbstractWindowManagerTest >> runCaseManaged [

	super runCaseManaged .
	self runCase
]

{ #category : 'running' }
AbstractWindowManagerTest >> setUp [

	super setUp.
	manager := MorphicWindowManager new
]

{ #category : 'running' }
AbstractWindowManagerTest >> tearDown [ 

	manager ensureDeleteAll.
	super tearDown

	
]
