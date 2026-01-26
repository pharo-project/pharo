Class {
	#name : 'ClapParameterizedTest',
	#superclass : 'ClapParameterTest',
	#category : 'Clap-Tests-Unit',
	#package : 'Clap-Tests',
	#tag : 'Unit'
}

{ #category : 'testing' }
ClapParameterizedTest class >> isAbstract [
	^ self = ClapParameterizedTest
]

{ #category : 'tests' }
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

{ #category : 'tests' }
ClapParameterizedTest >> testAddPositionals [
	| subject pos1 pos2 |
	subject := self namedSubject.
	pos1 := (ClapPositional id: #positional1)
		canonicalName: 'ARG1';
		yourself.
	pos2 := (ClapPositional id: #positional2)
		canonicalName: 'ARG2';
		yourself.
	subject addAll: { pos1 . pos2 }.
	
	self assert: (subject at: #positional1) identicalTo: pos1.
	self assert: (subject at: #positional2) identicalTo: pos2.
	self assert: (subject positionals includes: pos1).
	self assert: (subject positionals includes: pos2).

]

{ #category : 'tests' }
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
