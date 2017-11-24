"
I'am a komit object representing a change in a class definition. 
I wrap MCModification instances.

Notes from a maintainer: It may be interesting to see if my behavior cannot be simply merged into the MC hierarchy instead of somehow duplicating. 
"
Class {
	#name : #KomitDefinition,
	#superclass : #KomitObject,
	#instVars : [
		'definition',
		'operation'
	],
	#classInstVars : [
		'definitions'
	],
	#category : #'Komitter-Models'
}

{ #category : #'instance creation' }
KomitDefinition class >> definition: definition [

	^ self new
		definition: definition;
		yourself
]

{ #category : #'class initialization' }
KomitDefinition class >> initialize [

	definitions := Dictionary new
]

{ #category : #comparing }
KomitDefinition >> <= aKomitObject [ 

	^ self definition <= aKomitObject definition
]

{ #category : #accessing }
KomitDefinition >> definition [
	^ definition
]

{ #category : #accessing }
KomitDefinition >> definition: anObject [
	
	definition := anObject
]

{ #category : #testing }
KomitDefinition >> isCommitable [

	^ true
]

{ #category : #testing }
KomitDefinition >> isDefinition [

	^ true
]

{ #category : #'*Komitter-UI' }
KomitDefinition >> koDestinationText [
	
	^ self operation koDestinationText
]

{ #category : #'*Komitter-UI' }
KomitDefinition >> koSourceText [

	^ self operation koSourceText
]

{ #category : #accessing }
KomitDefinition >> operation [
	^ operation
]

{ #category : #protocol }
KomitDefinition >> operation: anOperation [
 
	operation := anOperation
]
