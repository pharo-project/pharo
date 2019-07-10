"
I represent string pattern using substring which tested strings are supposed to include.

I can be case sensitive or not depending on my variable.
To create my instances you can use following messages:
	ClySubstringPattern with: 'expected substring' caseSensitive: true
By default my instances are not case sensitive.	
	
Internal Representation and Key Implementation Points.

    Instance Variables
	isCaseSensitive:		<Boolean>
"
Class {
	#name : #ClySubstringPattern,
	#superclass : #ClyStringPattern,
	#instVars : [
		'isCaseSensitive'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClySubstringPattern class >> with: patternString caseSensitive: aBool [ 
	^(self with: patternString)
		isCaseSensitive: aBool
]

{ #category : #comparing }
ClySubstringPattern >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].

	^ isCaseSensitive = anObject isCaseSensitive
]

{ #category : #comparing }
ClySubstringPattern >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: isCaseSensitive hash
]

{ #category : #initialization }
ClySubstringPattern >> initialize [
	super initialize.
	
	isCaseSensitive := false
]

{ #category : #accessing }
ClySubstringPattern >> isCaseSensitive [
	^ isCaseSensitive
]

{ #category : #accessing }
ClySubstringPattern >> isCaseSensitive: anObject [
	isCaseSensitive := anObject
]

{ #category : #testing }
ClySubstringPattern >> matches: aString [
	^aString includesSubstring: value caseSensitive: isCaseSensitive 
]
