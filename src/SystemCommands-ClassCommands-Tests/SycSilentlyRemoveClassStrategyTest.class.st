"
A SycSilentlyRemoveClassStrategyTest is a test class for testing the behavior of SycSilentlyRemoveClassStrategy
"
Class {
	#name : #SycSilentlyRemoveClassStrategyTest,
	#superclass : #TestCase,
	#category : #'SystemCommands-ClassCommands-Tests'
}

{ #category : #tests }
SycSilentlyRemoveClassStrategyTest >> testExecutingStrategy [
	| class className command strategy |
	class := ClassFactoryForTestCase new newClass.
	className := class name.
	command := SycRemoveClassCommand for: { class }.
	strategy := SycSilentlyRemoveClassStrategy new.
	strategy execute: command.
	self deny: (Smalltalk hasClassNamed: className)
]
