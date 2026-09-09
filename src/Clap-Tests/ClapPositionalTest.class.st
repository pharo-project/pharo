Class {
	#name : 'ClapPositionalTest',
	#superclass : 'ClapParameterTest',
	#category : 'Clap-Tests-Unit',
	#package : 'Clap-Tests',
	#tag : 'Unit'
}

{ #category : 'accessing' }
ClapPositionalTest >> classToTest [
	^ ClapPositional
]

{ #category : 'tests' }
ClapPositionalTest >> testMatches [
	| subject argv ctx match |
	subject := self namedSubject.
	argv := { 'bar'. #remainder } readStream.
	ctx := ClapContext new.
	
	match := subject matchOn: argv in: ctx.
	
	self deny: match isMismatch.
	self assert: match specification identicalTo: subject.
	self assert: match parent identicalTo: ctx.
	self assert: match word equals: 'bar'.
	self assert: argv next equals: #remainder
]
