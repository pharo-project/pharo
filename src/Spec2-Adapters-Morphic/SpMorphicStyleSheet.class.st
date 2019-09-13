"
I'm something like a stylesheet. 
Each stylesheet can has a set of properties.
A property can also be another stylesheer (a class).

StyleSheets can be applied to morphs (see #applyTo:)

There is a default stylesheet which is the one that spec uses. See #newDefault method.
See concrete properties to see how this work.
"
Class {
	#name : #SpMorphicStyleSheet,
	#superclass : #Object,
	#instVars : [
		'properties'
	],
	#classVars : [
		'Current'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'instance creation' }
SpMorphicStyleSheet class >> current [

	^ Current ifNil: [ Current := self newDefault ]
]

{ #category : #'instance creation' }
SpMorphicStyleSheet class >> fromDictionary: aDictionary [

	^ self basicNew 
		initializeWith: aDictionary;
		yourself
	
]

{ #category : #'instance creation' }
SpMorphicStyleSheet class >> fromProperties: anArrayofProperties [

	^ self fromDictionary: (anArrayofProperties collect: [ :each | each name -> each ]) asDictionary
]

{ #category : #theme }
SpMorphicStyleSheet class >> newDefault [
	
	^ SpMorphicDefaultStyleSheet new
]

{ #category : #initialization }
SpMorphicStyleSheet class >> reset [ 
	<script>
	
	Current := nil
]

{ #category : #accessing }
SpMorphicStyleSheet class >> theme [
	^ Smalltalk ui theme
]

{ #category : #accessing }
SpMorphicStyleSheet >> addClass: aSymbol style: aStyleSheet [

	self 
		propertyAt: aSymbol 
		put: aStyleSheet
]

{ #category : #accessing }
SpMorphicStyleSheet >> addClass: aSymbol with: anArray [

	self 
		addClass: aSymbol 
		style: (SpMorphicStyleSheet fromProperties: anArray)
]

{ #category : #operations }
SpMorphicStyleSheet >> applyTo: aMorph [ 

	aMorph setProperty: #style toValue: self.
	properties valuesDo: [ :each | 
		each applyTo: aMorph ]
]

{ #category : #accessing }
SpMorphicStyleSheet >> atClass: aName [

	^ self 
		propertyAt: aName 
		ifAbsentPut: [ SpMorphicStyleSheet new ]
]

{ #category : #style }
SpMorphicStyleSheet >> extent [

	^ self 
		propertyAt: #extent 
		ifAbsent: [ SpMorphicStyleSheetExtent default ]
]

{ #category : #style }
SpMorphicStyleSheet >> height [

	^ self extent height
]

{ #category : #initialization }
SpMorphicStyleSheet >> initialize [

	super initialize.
	properties := Dictionary new
]

{ #category : #initialization }
SpMorphicStyleSheet >> initializeWith: aDictionary [ 

	super initialize.
	properties := aDictionary
]

{ #category : #accessing }
SpMorphicStyleSheet >> properties [ 

	^ properties
]

{ #category : #accessing }
SpMorphicStyleSheet >> propertyAt: aName [

	^ self propertyAt: aName ifAbsent: [ nil ]
]

{ #category : #accessing }
SpMorphicStyleSheet >> propertyAt: aName ifAbsent: aBlock [

	^ self properties 
		at: aName 
		ifAbsent: aBlock
]

{ #category : #accessing }
SpMorphicStyleSheet >> propertyAt: aName ifAbsentPut: aBlock [

	^ self properties 
		at: aName 
		ifAbsentPut: aBlock
]

{ #category : #accessing }
SpMorphicStyleSheet >> propertyAt: aName put: aValue [

	^ self properties 
		at: aName 
		put: aValue
]

{ #category : #accessing }
SpMorphicStyleSheet >> theme [
	^ self class theme
]

{ #category : #style }
SpMorphicStyleSheet >> width [

	^ self extent width
]
