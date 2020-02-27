"
This is the abstract class for container-based elements.
Elements are separated in groups by kind (e.g classes, methods, pools, etc.)
Subclasses have to define the kind of collection for a particular group of entities.
For a container the use of a dictionary or another collection (e.g. SortedCollection) to store group of elements is independent.


"
Class {
	#name : #RGAbstractContainer,
	#superclass : #RGDefinition,
	#instVars : [
		'elements'
	],
	#category : #'Ring-Definitions-Containers-Base'
}

{ #category : #'adding/removing' }
RGAbstractContainer >> addElement: anObject [

	anObject addInContainer: self
]

{ #category : #'adding/removing' }
RGAbstractContainer >> addElement: anObject in: aCollection [

	aCollection isDictionary
		ifTrue: [ aCollection at: anObject fullName put: anObject ]
		ifFalse:[ aCollection add: anObject ]
]

{ #category : #'lookup elements' }
RGAbstractContainer >> elementNamed: elementName [
	| aSymbol found |
	
	aSymbol:= elementName asSymbol.
	elements do:[ :collection| 		
		(found:= self elementNamed: aSymbol in: collection) notNil ifTrue:[ ^found ] ].
	^nil
]

{ #category : #'lookup elements' }
RGAbstractContainer >> elementNamed: elementName in: aCollection [
	| aSymbol |
	
	aSymbol:= elementName asSymbol.
	^aCollection isDictionary
		ifTrue: [ aCollection at: aSymbol ifAbsent: [ nil ] ]
		ifFalse:[ aCollection detect:[ :each| each fullName = aSymbol ] ifNone:[ nil ] ]
]

{ #category : #accessing }
RGAbstractContainer >> elements [
	"Retrieves the elements"
	
	^elements
]

{ #category : #'managing elements groups' }
RGAbstractContainer >> elementsCategorized: aSymbol [
	"Retrieves a group of elements.
	If does not exist set an Set"
	
	^elements at: aSymbol ifAbsentPut:[ Set ]
]

{ #category : #'managing elements groups' }
RGAbstractContainer >> elementsCategorized: aSymbol with: aCollection [
	"Allows to define other groups of elements with a particular kind of collection"
	
	elements at: aSymbol put: aCollection
]

{ #category : #'iterating elements' }
RGAbstractContainer >> elementsDo: aBlock [
	elements valuesDo: [ :collection | collection do: [ :each | aBlock value: each ] ]
]

{ #category : #initialization }
RGAbstractContainer >> flushElements [

	elements:= IdentityDictionary new
]

{ #category : #testing }
RGAbstractContainer >> includesElement: anRGDefinition [

	^anRGDefinition isIncludedInContainer: self
]

{ #category : #testing }
RGAbstractContainer >> includesElementNamed: elementName [

	^ elements anySatisfy: [ :collection | self includesElementNamed: elementName asSymbol in: collection ]
]

{ #category : #testing }
RGAbstractContainer >> includesElementNamed: elementName in: aCollection [

	^aCollection isDictionary
		ifTrue: [ aCollection includesKey:  elementName asSymbol ]
		ifFalse:[ aCollection anySatisfy: [ :each| each name = elementName asSymbol ] ]
]

{ #category : #initialization }
RGAbstractContainer >> initialize [

	super initialize.
	elements:= IdentityDictionary new.
]

{ #category : #'adding/removing' }
RGAbstractContainer >> removeElement: anObject [

	anObject removeFromContainer: self
]

{ #category : #'adding/removing' }
RGAbstractContainer >> removeElement: anObject from: aCollection [

	aCollection isDictionary
		ifTrue: [ aCollection removeKey: anObject fullName ifAbsent:[ ] ]
		ifFalse:[ aCollection remove: anObject ifAbsent:[ ] ]
]

{ #category : #'managing elements groups' }
RGAbstractContainer >> removeElementsCategorized: aSymbol [
	"Deletes a group of elements"
	
	elements removeKey: aSymbol ifAbsent:[ ]
]
