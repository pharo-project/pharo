Class {
	#name : #GTPlayPagePersistence,
	#superclass : #Object,
	#instVars : [
		'page'
	],
	#category : #'GT-Playground'
}

{ #category : #testing }
GTPlayPagePersistence class >> isDeprecated [
	^ true
]

{ #category : #testing }
GTPlayPagePersistence >> isStash [
	^ false
]

{ #category : #accessing }
GTPlayPagePersistence >> page [
	^ page
]

{ #category : #accessing }
GTPlayPagePersistence >> page: anObject [
	page := anObject
]
