Class {
	#name : #ClapNamedMatch,
	#superclass : #ClapCompositeMatch,
	#instVars : [
		'keyword'
	],
	#category : #'Clap-Core-Activation'
}

{ #category : #matching }
ClapNamedMatch >> completeMatchOn: aStream [
	keyword := aStream next.
	startIndex := aStream position.
	self matchChildrenOn: aStream
]

{ #category : #matching }
ClapNamedMatch >> word [
	^ keyword
]
