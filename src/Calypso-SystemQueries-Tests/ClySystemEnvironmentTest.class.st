"
A ClySystemEnvironmentTest is a test class for testing the behavior of ClySystemEnvironment
"
Class {
	#name : 'ClySystemEnvironmentTest',
	#superclass : 'TestCase',
	#instVars : [
		'dummySystemEnvironment'
	],
	#category : 'Calypso-SystemQueries-Tests-Domain',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Domain'
}

{ #category : 'running' }
ClySystemEnvironmentTest >> setUp [
	super setUp.
	dummySystemEnvironment := ClySystemEnvironment new globals: Smalltalk globals.
]

{ #category : 'tests' }
ClySystemEnvironmentTest >> testCreateClass [
	| newClassDefinitionString |
	
	newClassDefinitionString := 'Object << #ClySystemEnvironmentClassForTest
	slots: {  };
	package: ''Calypso-SystemQueries-Tests'''.
	dummySystemEnvironment 
		compileANewClassFrom: newClassDefinitionString notifying: nil startingFrom: nil.
	
	self assert: (Smalltalk globals hasClassNamed: #ClySystemEnvironmentClassForTest).
	Smalltalk globals removeClassNamed: #ClySystemEnvironmentClassForTest
]

{ #category : 'tests' }
ClySystemEnvironmentTest >> testCreateTrait [
	| newTraitDefinitionString |
	
	newTraitDefinitionString := 'Trait << #ClySystemEnvironmentTraitForTest
	package: ''Calypso-SystemQueries-Tests'''.
	dummySystemEnvironment 
		compileANewClassFrom: newTraitDefinitionString notifying: nil startingFrom: nil.
	
	self assert: (Smalltalk globals hasClassOrTraitNamed: #ClySystemEnvironmentTraitForTest).
	Smalltalk globals removeClassNamed: #ClySystemEnvironmentTraitForTest.
]

{ #category : 'tests' }
ClySystemEnvironmentTest >> testExtractClassNameFrom [

	self
		assert: (dummySystemEnvironment extractNameFrom: 'Object << #ClySystemEnvironment
	slots: { #name. #globals. #changesAnnouncer. #packageOrganizer. #projectManager };
	package: ''Calypso-SystemQueries-Domain''')
		equals: 'ClySystemEnvironment'.
	self assert: (dummySystemEnvironment extractNameFrom: 'Object << #ClySystemEnvironment') equals: 'ClySystemEnvironment'.
	self assert: (dummySystemEnvironment extractNameFrom: 'Object<<#ClySystemEnvironment') equals: 'ClySystemEnvironment'.
	self
		assert: (dummySystemEnvironment extractNameFrom: 'Object << #ClySystemEnvironment
	slots: {};
	sharedVariables: {};
	package: ''Calypso-SystemQueries-Domain''')
		equals: 'ClySystemEnvironment'.
	self
		assert: (dummySystemEnvironment extractNameFrom: 'Trait << #TMyTrait
	traits: {};
	slots: {};
	tag: '''' ;
	package: ''Calypso-SystemQueries-Tests-Domain''')
		equals: 'TMyTrait'
]
