"
I'm something like a stylesheet. 
Each stylesheet can has a set of properties.
A property can also be another stylesheer (a class).

StyleSheets can be applied to morphs (see #applyTo:)

There is a default stylesheet which is the one that spec uses. See #newDefault method.
See concrete properties to see how this work.
"
Class {
	#name : #MorphicStyleSheet,
	#superclass : #Object,
	#instVars : [
		'properties'
	],
	#classVars : [
		'Current'
	],
	#category : #'Spec-MorphicAdapters-StyleSheet'
}

{ #category : #'instance creation' }
MorphicStyleSheet class >> current [

	^ Current ifNil: [ Current := self newDefault ]
]

{ #category : #'instance creation' }
MorphicStyleSheet class >> fromDictionary: aDictionary [

	^ self basicNew 
		initializeWith: aDictionary;
		yourself
	
]

{ #category : #'instance creation' }
MorphicStyleSheet class >> fromProperties: anArrayofProperties [

	^ self fromDictionary: (anArrayofProperties collect: [ :each | each name -> each ]) asDictionary
]

{ #category : #theme }
MorphicStyleSheet class >> newDefault [
	
	^ MorphicDefaultStyleSheet new
]

{ #category : #utils }
MorphicStyleSheet class >> reset [ 
	<script>
	
	Current := nil
]

{ #category : #accessing }
MorphicStyleSheet >> addClass: aSymbol style: aStyleSheet [

	self 
		propertyAt: aSymbol 
		put: aStyleSheet
]

{ #category : #accessing }
MorphicStyleSheet >> addClass: aSymbol with: anArray [

	self 
		addClass: aSymbol 
		style: (MorphicStyleSheet fromProperties: anArray)
]

{ #category : #operations }
MorphicStyleSheet >> applyTo: aMorph [ 

	aMorph setProperty: #style toValue: self.
	properties valuesDo: [ :each | 
		each applyTo: aMorph ]
]

{ #category : #accessing }
MorphicStyleSheet >> atClass: aName [

	^ self 
		propertyAt: aName 
		ifAbsentPut: [ MorphicStyleSheet new ]
]

{ #category : #style }
MorphicStyleSheet >> extent [

	^ self propertyAt: #extent
]

{ #category : #style }
MorphicStyleSheet >> height [

	^ self extent height
]

{ #category : #initialization }
MorphicStyleSheet >> initialize [

	super initialize.
	properties := Dictionary new
]

{ #category : #initialization }
MorphicStyleSheet >> initializeWith: aDictionary [ 

	super initialize.
	properties := aDictionary
]

{ #category : #accessing }
MorphicStyleSheet >> properties [ 

	^ properties
]

{ #category : #accessing }
MorphicStyleSheet >> propertyAt: aName [

	^ self propertyAt: aName ifAbsent: [ nil ]
]

{ #category : #accessing }
MorphicStyleSheet >> propertyAt: aName ifAbsent: aBlock [

	^ self properties 
		at: aName 
		ifAbsent: aBlock
]

{ #category : #accessing }
MorphicStyleSheet >> propertyAt: aName ifAbsentPut: aBlock [

	^ self properties 
		at: aName 
		ifAbsentPut: aBlock
]

{ #category : #accessing }
MorphicStyleSheet >> propertyAt: aName put: aValue [

	^ self properties 
		at: aName 
		put: aValue
]

{ #category : #style }
MorphicStyleSheet >> width [

	^ self extent width
]
