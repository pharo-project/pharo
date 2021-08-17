"
I am a superclass of most objects within Pier. I hold a dictionary of properties, so that users can easily annotate me with new values. I am visitable.
"
Class {
	#name : #PRObject,
	#superclass : #PRBasicObject,
	#instVars : [
		'properties'
	],
	#category : #'Pillar-Core-Roots'
}

{ #category : #testing }
PRObject class >> isAbstract [
	^ true
]

{ #category : #'instance creation' }
PRObject class >> new [
	"Create a new instance of this class and checks if it isn't abstract."

	self isAbstract
		ifTrue: [ self error: self name , ' is abstract.' ].
	^ super new
]

{ #category : #reflection }
PRObject class >> withAllConcreteClasses [
	^ Array streamContents: [ :stream |
		self withAllConcreteClassesDo: [ :each |
			stream nextPut: each ] ]
]

{ #category : #reflection }
PRObject class >> withAllConcreteClassesDo: aBlock [
	self
		withAllSubclassesDo: [ :each | 
			each isAbstract
				ifFalse: [ aBlock value: each ] ]
]

{ #category : #visiting }
PRObject >> accept: aVisitor [
	<ignoreForCoverage>
	"Dispatch to ==aVisitor== depending on the receiver."
]

{ #category : #private }
PRObject >> errorPropertyNotFound: aSelector [
	PRPropertyError signal: 'Property ' , aSelector , ' not found.'
]

{ #category : #testing }
PRObject >> hasProperty: aKey [
	"Test if the property ==aKey== is defined within the receiver."

	^ self properties includesKey: aKey
]

{ #category : #copying }
PRObject >> postCopy [
	super postCopy.
	properties := properties copy
]

{ #category : #printing }
PRObject >> printOn: aStream [
	<ignoreForCoverage>
	super printOn: aStream.
	aStream nextPut: $[; print: self identityHash; nextPut: $]
]

{ #category : #'accessing - properties' }
PRObject >> properties [
	"Answer the property dictionary of the receiver."

	^ properties ifNil: [ properties := Dictionary new ]
]

{ #category : #'accessing - properties' }
PRObject >> properties: aDictionary [
	
	properties := aDictionary
]

{ #category : #'accessing - properties' }
PRObject >> propertyAt: aKey [
	"Answer the value of the property ==aKey==, raises an error if the property doesn't exist."

	^ self propertyAt: aKey ifAbsent: [ self errorPropertyNotFound: aKey ]
]

{ #category : #'accessing - properties' }
PRObject >> propertyAt: aKey ifAbsent: aBlock [
	"Answer the value of the property ==aKey==, or the result of ==aBlock== if the property doesn't exist."

	^ self properties at: aKey ifAbsent: aBlock
]

{ #category : #'accessing - properties' }
PRObject >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the value of the property ==aKey==, or if the property doesn't exist adds and answers the result of evaluating ==aBlock==."

	^ self properties at: aKey ifAbsentPut: aBlock
]

{ #category : #'accessing - properties' }
PRObject >> propertyAt: aKey put: aValue [
	"Adds or replaces the property ==aKey== with ==aValue==."

	^ self properties at: aKey put: aValue
]
