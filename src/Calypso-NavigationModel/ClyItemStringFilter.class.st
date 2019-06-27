"
I am a root of hierarchy of filters which use string pattern to filter objects.
Concrete filter function is still responsibility of subclasses.

I only define correct comparison with other filters and suitable constructors:

	StringFilterClass regexPattern: aRegexString.
	StringFilterClass substringPattern: aSubString
	StringFilterClass pattern: aStringPattern
	
Internal Representation and Key Implementation Points.

    Instance Variables
	pattern:		<ClyStringPattern>
"
Class {
	#name : #ClyItemStringFilter,
	#superclass : #ClyItemFilter,
	#instVars : [
		'pattern'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClyItemStringFilter class >> pattern: aStringPattern [
	^self new 
		pattern: aStringPattern
]

{ #category : #'instance creation' }
ClyItemStringFilter class >> regexPattern: regexString [
	^self 	pattern: (ClyRegexPattern with: regexString)
]

{ #category : #'instance creation' }
ClyItemStringFilter class >> substringPattern: aString [
	^self 	pattern: (ClySubstringPattern with: aString)
]

{ #category : #comparing }
ClyItemStringFilter >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ pattern = anObject pattern
]

{ #category : #comparing }
ClyItemStringFilter >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ pattern hash
]

{ #category : #accessing }
ClyItemStringFilter >> pattern [
	^ pattern
]

{ #category : #accessing }
ClyItemStringFilter >> pattern: anObject [
	pattern := anObject
]

{ #category : #printing }
ClyItemStringFilter >> printCriteria [

	^pattern value
]

{ #category : #printing }
ClyItemStringFilter >> printOn: aStream [
	super printOn: aStream.
	
	aStream nextPut: $(.
	pattern printOn: aStream.
	aStream nextPut: $)
]
