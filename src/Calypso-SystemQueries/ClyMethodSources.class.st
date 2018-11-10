"
I return all methods which source code includes particular string pattern.
For example it can be simple substring or regex expression. 
But generally pattern is represented by ClyStringPattern subclasses.

To create my instances use following methods:

	ClyMethodSources withString: 'probe string'.
	ClyMethodSources withString: 'probe string' caseSensitive: true.
	ClyMethodSources filteredBy: aStringPattern
	
Internal Representation and Key Implementation Points.

    Instance Variables
	pattern:		<ClyStringPattern>
"
Class {
	#name : #ClyMethodSources,
	#superclass : #ClyMethodQuery,
	#instVars : [
		'pattern'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyMethodSources class >> filteredBy: aStringPattern [
	^self new 
		pattern: aStringPattern
]

{ #category : #'instance creation' }
ClyMethodSources class >> filteredBy: aStringPattern from: aScope [
	^(self from: aScope)
		pattern: aStringPattern
]

{ #category : #'instance creation' }
ClyMethodSources class >> withString: aString [

	^self withString: aString caseSensitive: false
]

{ #category : #'instance creation' }
ClyMethodSources class >> withString: aString caseSensitive: aBool [

	^self filteredBy: (ClySubstringPattern with: aString caseSensitive: aBool)
]

{ #category : #'instance creation' }
ClyMethodSources class >> withString: aString caseSensitive: aBool from: aScope [

	^self 
		filteredBy: (ClySubstringPattern with: aString caseSensitive: aBool)
		from: aScope 
]

{ #category : #'instance creation' }
ClyMethodSources class >> withString: aString from: aScope [

	^self withString: aString caseSensitive: false from: aScope
]

{ #category : #comparing }
ClyMethodSources >> = anObject [
	"Answer whether the receiver and anObject represent the same object."
	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ pattern = anObject pattern
]

{ #category : #printing }
ClyMethodSources >> description [

	^'methods sources with ', pattern value printString
]

{ #category : #comparing }
ClyMethodSources >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: pattern hash
]

{ #category : #accessing }
ClyMethodSources >> pattern [
	^ pattern
]

{ #category : #accessing }
ClyMethodSources >> pattern: anObject [
	pattern := anObject
]

{ #category : #printing }
ClyMethodSources >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.

	^aStream nextPutAll: pattern value
]

{ #category : #testing }
ClyMethodSources >> selectsMethod: aMethod [

	^pattern matches: aMethod sourceCode
]
