"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'ForEventTestMock',
	#superclass : 'Object',
	#instVars : [
		'stringMorph',
		'counter'
	],
	#category : 'Morphic-Tests-Event',
	#package : 'Morphic-Tests',
	#tag : 'Event'
}

{ #category : 'accessing' }
ForEventTestMock >> counter [
	^ counter
]

{ #category : 'accessing' }
ForEventTestMock >> counter: anObject [
	counter := anObject
]

{ #category : 'initialization' }
ForEventTestMock >> initialize [
	super initialize.
	counter := 0
]

{ #category : 'accessing' }
ForEventTestMock >> mockStringMorph [
	^ StringMorph new
		contents: 'Test';
		on: #mouseEnter send: #mouseEnter:from: to: self;
		yourself
]

{ #category : 'accessing' }
ForEventTestMock >> mouseEnter: a from: b [
	counter := counter + 1
]

{ #category : 'accessing' }
ForEventTestMock >> stringMorph [
	^ stringMorph ifNil: [ stringMorph := self mockStringMorph ]
]
