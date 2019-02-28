Class {
	#name : #ClapParameterTest,
	#superclass : #TestCase,
	#instVars : [
		'subject'
	],
	#category : #'Clap-Tests-Unit'
}

{ #category : #testing }
ClapParameterTest class >> isAbstract [
	^ self == ClapParameterTest
]

{ #category : #accessing }
ClapParameterTest >> classToTest [
	^ self subclassResponsibility
]

{ #category : #running }
ClapParameterTest >> setUp [
	super setUp.
	subject := self classToTest withName: self subjectName
]

{ #category : #running }
ClapParameterTest >> subjectName [
	^ 'foo'
]

{ #category : #tests }
ClapParameterTest >> testCanonicalName [
	self
		assert: subject canonicalName
		equals: self subjectName
]

{ #category : #tests }
ClapParameterTest >> testMismatchesAtEnd [
	| argv match |
	argv := #() readStream.
	
	match := subject matchOn: argv.
	
	self assert: match isMismatch.
	self
		assert: match specification
		identicalTo: subject
]
