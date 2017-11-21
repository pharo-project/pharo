"
I represent any commitable object, mainly a simple commit or a slice
"
Class {
	#name : #KomitableObject,
	#superclass : #Object,
	#instVars : [
		'author',
		'entities',
		'message',
		'timestamp',
		'remote',
		'unselectedEntities'
	],
	#category : #Komitter-Models
}

{ #category : #accessing }
KomitableObject >> author [
	^ author
]

{ #category : #accessing }
KomitableObject >> author: anObject [
	author := anObject
]

{ #category : #protocol }
KomitableObject >> commitToMonticello [

	Komitter resetLastMessage.
	KomitStagingArea resetCurrent.

	MCKomitSubmitter new
		submitCommit: self
]

{ #category : #accessing }
KomitableObject >> entities [
	^ entities
]

{ #category : #accessing }
KomitableObject >> entities: aDictionary [
	entities := aDictionary
]

{ #category : #initialize }
KomitableObject >> initialize [

	super initialize.
	author := Author fullName.
	entities := Dictionary new.
	unselectedEntities := Dictionary new.
	message := ''.
	timestamp := DateAndTime now.
]

{ #category : #accessing }
KomitableObject >> message [
	^ message
]

{ #category : #accessing }
KomitableObject >> message: anObject [
	message := anObject
]

{ #category : #accessing }
KomitableObject >> remote [
	^ remote
]

{ #category : #accessing }
KomitableObject >> remote: anObject [
	remote := anObject
]

{ #category : #protocol }
KomitableObject >> repository [
	
	^ self remote remote
]

{ #category : #accessing }
KomitableObject >> timestamp [
	^ timestamp
]

{ #category : #accessing }
KomitableObject >> timestamp: anObject [
	timestamp := anObject
]

{ #category : #accessing }
KomitableObject >> unselectedEntities [
	^ unselectedEntities
]

{ #category : #accessing }
KomitableObject >> unselectedEntities: anObject [
	unselectedEntities := anObject
]
