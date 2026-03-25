"
Simple fillstyle that draws a (potentially translucent) form at the specified origin. Direction and normal are unused.
"
Class {
	#name : 'ImageFillStyle',
	#superclass : 'OrientedFillStyle',
	#instVars : [
		'formSet',
		'extent',
		'offset'
	],
	#category : 'Graphics-Canvas-Fills',
	#package : 'Graphics-Canvas',
	#tag : 'Fills'
}

{ #category : 'instance creation' }
ImageFillStyle class >> form: aForm [

	^ self formSet: (FormSet form: aForm)
]

{ #category : 'instance creation' }
ImageFillStyle class >> formSet: aFormSet [
	"Answer a new instance of the receiver with the given form set."

	^self new formSet: aFormSet
]

{ #category : 'comparing' }
ImageFillStyle >> = anOrientedFillStyle [
	"Answer whether equal."

	^super = anOrientedFillStyle
		and: [(self form = anOrientedFillStyle form) and: [ offset = anOrientedFillStyle offset]]
]

{ #category : 'converting' }
ImageFillStyle >> asAthensPaintOn: anAthensCanvas [

	^ (anAthensCanvas cacheAt: self ifAbsentPut: [
		anAthensCanvas surface
		createFormPaint: self form ])
		origin: origin
]

{ #category : 'converting' }
ImageFillStyle >> asColor [
	"Answer transparent, no other reasonable interpretation."

	^Color transparent
]

{ #category : 'accessing' }
ImageFillStyle >> extent [
	"Answer the value of extent"

	^ extent
]

{ #category : 'accessing' }
ImageFillStyle >> extent: anObject [
	"Set the value of extent"

	extent := anObject
]

{ #category : 'drawing' }
ImageFillStyle >> fillRectangle: aRectangle on: aCanvas [
	"Fill the given rectangle on the given canvas with the receiver."

	self extent
		ifNil: [aCanvas
				translucentFormSet: self formSet
				at: self origin]
		ifNotNil: [aCanvas clipBy: (self origin + self offset extent: self extent) during: [:c |
					c
						translucentFormSet: self formSet
						at: self origin]]
]

{ #category : 'accessing' }
ImageFillStyle >> form [

	^ formSet asForm
]

{ #category : 'accessing' }
ImageFillStyle >> form: aForm [

	self formSet: (FormSet form: aForm)
]

{ #category : 'accessing' }
ImageFillStyle >> formSet [
	"Answer the value of formSet"

	^ formSet
]

{ #category : 'accessing' }
ImageFillStyle >> formSet: aFormSet [
	"Set the value of formSet"

	formSet := aFormSet.
	self direction: aFormSet extent
]

{ #category : 'comparing' }
ImageFillStyle >> hash [
	"Hash is implemented because #= is implemented."

	^super hash bitXor: self form hash
]

{ #category : 'initialization' }
ImageFillStyle >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		origin: 0@0;
		offset: 0@0
]

{ #category : 'accessing' }
ImageFillStyle >> offset [
	"Answer the value of offset"

	^ offset
]

{ #category : 'accessing' }
ImageFillStyle >> offset: anObject [
	"Set the value of offset"

	offset := anObject
]
