"
I am a query of class comments

I filter available classes using particular string pattern which class comment should satisfy.
For example it can be simple substring or regex expression. 
But generally pattern is represented by ClyStringPattern subclasses.

To create my instances use following methods:

	ClyClassComment withString: 'probe string'.
	ClyClassComment withString: 'probe string' caseSensitive: true.
	ClyClassComment filteredBy: aStringPattern
	
Internal Representation and Key Implementation Points.

    Instance Variables
	pattern:		<ClyStringPattern>
"
Class {
	#name : #ClyClassCommentsQuery,
	#superclass : #ClyClassQuery,
	#instVars : [
		'pattern'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyClassCommentsQuery class >> filteredBy: aStringPattern [
	^self new 
		pattern: aStringPattern
]

{ #category : #'instance creation' }
ClyClassCommentsQuery class >> filteredBy: aStringPattern from: aScope [
	^(self from: aScope)
		pattern: aStringPattern
]

{ #category : #'items type' }
ClyClassCommentsQuery class >> resultItemsType [
	^ClyClassComment
]

{ #category : #'instance creation' }
ClyClassCommentsQuery class >> withString: aString [

	^self withString: aString caseSensitive: false
]

{ #category : #'instance creation' }
ClyClassCommentsQuery class >> withString: aString caseSensitive: aBool [

	^self filteredBy: (ClySubstringPattern with: aString caseSensitive: aBool)
]

{ #category : #'instance creation' }
ClyClassCommentsQuery class >> withString: aString caseSensitive: aBool from: aScope [

	^self 
		filteredBy: (ClySubstringPattern with: aString caseSensitive: aBool)
		from: aScope 
]

{ #category : #'instance creation' }
ClyClassCommentsQuery class >> withString: aString from: aScope [

	^self withString: aString caseSensitive: false from: aScope
]

{ #category : #comparing }
ClyClassCommentsQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."
	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ pattern = anObject pattern
]

{ #category : #execution }
ClyClassCommentsQuery >> buildResult: aQueryResult [
	| classComments |
	classComments := OrderedCollection new.
	
	scope classesDo: [ :each | 
		(self selectsClass: each) ifTrue: [
			classComments add: (ClyClassComment of: each)]].
	
	aQueryResult fillWith: classComments
]

{ #category : #printing }
ClyClassCommentsQuery >> description [

	^'class comments with ', pattern value printString
]

{ #category : #comparing }
ClyClassCommentsQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: pattern hash
]

{ #category : #accessing }
ClyClassCommentsQuery >> pattern [
	^ pattern
]

{ #category : #accessing }
ClyClassCommentsQuery >> pattern: anObject [
	pattern := anObject
]

{ #category : #printing }
ClyClassCommentsQuery >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.

	aStream nextPutAll: pattern value
]

{ #category : #testing }
ClyClassCommentsQuery >> selectsClass: aClass [

	^aClass hasComment and: [ pattern matches: aClass comment]
]
