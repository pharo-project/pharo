Class {
	#name : #MockObjectForEventTests,
	#superclass : #Object,
	#instVars : [
		'stringMorph',
		'counter'
	],
	#category : #'Morphic-Tests-Event'
}

{ #category : #accessing }
MockObjectForEventTests >> counter [
	^ counter
]

{ #category : #accessing }
MockObjectForEventTests >> counter: anObject [
	counter := anObject
]

{ #category : #initialization }
MockObjectForEventTests >> initialize [
	super initialize.
	counter := 0
]

{ #category : #accessing }
MockObjectForEventTests >> mockStringMorph [
	^ StringMorph new
		contents: 'Test';
		on: #mouseEnter send: #mouseEnter:from: to: self;
		yourself
]

{ #category : #accessing }
MockObjectForEventTests >> mouseEnter: a from: b [
	counter := counter + 1
]

{ #category : #accessing }
MockObjectForEventTests >> stringMorph [
	^ stringMorph ifNil: [ stringMorph := self mockStringMorph ]
]
