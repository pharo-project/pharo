Class {
	#name : #SycRemoveKeepingSubclassesClassStrategyTest,
	#superclass : #TestCase,
	#instVars : [
		'class',
		'subclass',
		'factory'
	],
	#category : #'SystemCommands-ClassCommands-Tests'
}

{ #category : #running }
SycRemoveKeepingSubclassesClassStrategyTest >> setUp [
	| command strategy |
	super setUp.
	factory := ClassFactoryForTestCase new.
	class := factory newClass.
	subclass := factory newSubclassOf: class.
	command := SycRemoveClassCommand for: { class }.
	strategy := SycRemoveKeepingSubclassesClassStrategy new.
	strategy execute: command.
]

{ #category : #running }
SycRemoveKeepingSubclassesClassStrategyTest >> tearDown [
	factory cleanUp.
	super tearDown
]

{ #category : #tests }
SycRemoveKeepingSubclassesClassStrategyTest >> testExecutingStrategyDeletesTheClass [

	self deny: (Smalltalk hasClassNamed: class name)
]

{ #category : #tests }
SycRemoveKeepingSubclassesClassStrategyTest >> testExecutingStrategyKeepsTheSubclass [

	self
		assert: (Smalltalk hasClassNamed: subclass name);
		assert: subclass superclass identicalTo: factory defaultSuperclass
]
