"
I represent an explicit match over words in the argument sequence, which constitutes an explicit occurrence of my specification.

My instances have a value, given by the meaning block of my specification.
"
Class {
	#name : #ClapExplicit,
	#superclass : #ClapSubExpression,
	#instVars : [
		'startIndex'
	],
	#category : #'Clap-Core-Activation'
}

{ #category : #testing }
ClapExplicit class >> isAbstract [
	^ self == ClapExplicit
]

{ #category : #matching }
ClapExplicit >> completeMatchOn: aStream [
	self subclassResponsibility
]

{ #category : #testing }
ClapExplicit >> isExplicit [
	^ true
]

{ #category : #adding }
ClapExplicit >> recordIn: parentMatch [
	self parent: parentMatch.
	self parent addChild: self
]

{ #category : #accessing }
ClapExplicit >> span [
	^ self start to: self stop
]

{ #category : #accessing }
ClapExplicit >> start [
	^ startIndex
]

{ #category : #accessing }
ClapExplicit >> stop [
	^ self subclassResponsibility
]

{ #category : #evaluating }
ClapExplicit >> words [
	^ self context arguments
		collect: [ :str | str asByteArray utf8Decoded ]
		from: self start
		to: self stop
]

{ #category : #evaluating }
ClapExplicit >> wordsDo: aBlock [
	^ self context arguments
		from: self start
		to: self stop
		do: [ :str | aBlock value: str asByteArray utf8Decoded ]
]
