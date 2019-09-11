"
My subclasses represent specific kind of pattern to filter given strings.

Subclases should implement single method #matches: to check if string in argument is satisfied pattern.

If my subclasses define extra state the should implement comparison method according to my logic.

I keep actual pattern string in the value variable. 
Instances can be created using #with: message:
	ClySubstringPattern with: 'expected substring'

Internal Representation and Key Implementation Points.

    Instance Variables
	value:		<String>
"
Class {
	#name : #ClyStringPattern,
	#superclass : #Object,
	#instVars : [
		'value'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClyStringPattern class >> with: patternString [
	^self new 
		value: patternString 
]

{ #category : #comparing }
ClyStringPattern >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ value = anObject value
]

{ #category : #comparing }
ClyStringPattern >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ value hash
]

{ #category : #testing }
ClyStringPattern >> matches: aString [
	self subclassResponsibility 
]

{ #category : #printing }
ClyStringPattern >> printOn: aStream [
	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: value;
		nextPut: $)
]

{ #category : #accessing }
ClyStringPattern >> value [
	^ value
]

{ #category : #accessing }
ClyStringPattern >> value: anObject [
	value := anObject
]
