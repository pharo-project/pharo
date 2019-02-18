Class {
	#name : #ClapParameterizedTest,
	#superclass : #ClapParameterTest,
	#category : #'Clap-Tests-Unit'
}

{ #category : #testing }
ClapParameterizedTest class >> isAbstract [
	^ self == ClapParameterizedTest
]

{ #category : #tests }
ClapParameterizedTest >> testAddPositional [
	| pos |
	pos := ClapPositional withName: 'pos'.
	subject add: pos.
	
	self assert: (subject positionals includes: pos)
]
