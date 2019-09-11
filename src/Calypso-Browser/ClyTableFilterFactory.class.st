"
I responsible to create new filter instances of given itemFilterClass using specified string pattern.

I am a kind of hook to be able configure complex filters for fast table where it uses just a filter class. Fast table instantiates instances using #pattern: message which I define as filter creation method

Internal Representation and Key Implementation Points.

    Instance Variables
	itemFilterClass:		<ClyItemStringFilter class>
	stringPattern:		<ClyStringPatter>
"
Class {
	#name : #ClyTableFilterFactory,
	#superclass : #Object,
	#instVars : [
		'itemFilterClass',
		'stringPattern'
	],
	#category : #'Calypso-Browser-Table'
}

{ #category : #'instance creation' }
ClyTableFilterFactory class >> of: anItemStringFilterClass [
	^self new 
		itemFilterClass: anItemStringFilterClass
]

{ #category : #'instance creation' }
ClyTableFilterFactory class >> of: anItemStringFilterClass using: aStringPattern [
	^(self of: anItemStringFilterClass)
		stringPattern: aStringPattern
]

{ #category : #initialization }
ClyTableFilterFactory >> initialize [
	super initialize.
	
	stringPattern := ClySubstringPattern with: '' caseSensitive: false
]

{ #category : #accessing }
ClyTableFilterFactory >> itemFilterClass [
	^ itemFilterClass
]

{ #category : #accessing }
ClyTableFilterFactory >> itemFilterClass: anObject [
	itemFilterClass := anObject
]

{ #category : #'filter creation' }
ClyTableFilterFactory >> pattern: aString [
	"The method is called from fast table filter function to create new instance of item filter"
	| newPattern |
	newPattern := stringPattern copy.
	newPattern value: aString.
	^itemFilterClass pattern: newPattern 
]

{ #category : #accessing }
ClyTableFilterFactory >> stringPattern [
	^ stringPattern
]

{ #category : #accessing }
ClyTableFilterFactory >> stringPattern: anObject [
	stringPattern := anObject
]
