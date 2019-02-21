"
I am a node in the match tree.
My parent is either the match I'm a child of, or the context for the whole command invocation.
"
Class {
	#name : #ClapSubExpression,
	#superclass : #ClapExpression,
	#instVars : [
		'parent'
	],
	#category : #'Clap-Core-Activation'
}

{ #category : #testing }
ClapSubExpression class >> isAbstract [
	^ self == ClapSubExpression
]

{ #category : #enumerating }
ClapSubExpression >> allOccurrences [
	^ self parent
		occurrencesOf: self specification
]

{ #category : #enumerating }
ClapSubExpression >> allOccurrencesCollect: aBlock [
	^ self parent
		occurrencesOf: self specification
		collect: aBlock
]

{ #category : #enumerating }
ClapSubExpression >> allOccurrencesDo: aBlock [
	^ self parent
		occurrencesOf: self specification
		do: aBlock
]

{ #category : #accessing }
ClapSubExpression >> context [
	^ parent context
]

{ #category : #testing }
ClapSubExpression >> ifMatch: matchBlock ifMismatch: mismatchBlock [
	^ matchBlock cull: self
]

{ #category : #testing }
ClapSubExpression >> isExplicit [
	^ self subclassResponsibility
]

{ #category : #accessing }
ClapSubExpression >> parent [
	^ parent
]

{ #category : #accessing }
ClapSubExpression >> parent: aMatch [
	parent := aMatch
]
