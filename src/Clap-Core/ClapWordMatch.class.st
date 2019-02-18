Class {
	#name : #ClapWordMatch,
	#superclass : #ClapExplicit,
	#instVars : [
		'word'
	],
	#category : #'Clap-Core-Activation'
}

{ #category : #matching }
ClapWordMatch >> completeMatchOn: aStream [
	word := aStream next.
	startIndex := aStream position.
]

{ #category : #accessing }
ClapWordMatch >> stop [
	^ self start
]

{ #category : #accessing }
ClapWordMatch >> word [
	^ word
]
