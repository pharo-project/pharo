Class {
	#name : #ClapParameterizedTest,
	#superclass : #ClapParameterTest,
	#category : #'Clap-Tests-Unit'
}

{ #category : #testing }
ClapParameterizedTest class >> isAbstract [
	^ self = ClapParameterizedTest
]

{ #category : #tests }
ClapParameterizedTest >> testAddPositional [
	| subject pos |
	subject := self namedSubject.
	pos := (ClapPositional id: #myPositional)
		canonicalName: 'ARG';
		yourself.
	subject add: pos.
	
	self assert: (subject at: #myPositional) identicalTo: pos.
	self assert: (subject positionals includes: pos).

]

{ #category : #tests }
ClapParameterizedTest >> testIdentifierConflict [
	| subject first second |
	subject := self namedSubject.
	first := ClapPositional id: #theSame.
	second := ClapPositional id: #theSame.
	subject add: first.

	self
		should: [ subject add: second ]
		raise: Error

]
