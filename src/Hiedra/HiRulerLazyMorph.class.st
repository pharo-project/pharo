"
I'm a Morph that ask to a model a source form and a source rectangle to draw myself.
"
Class {
	#name : #HiRulerLazyMorph,
	#superclass : #Morph,
	#instVars : [
		'model',
		'key'
	],
	#category : 'Hiedra-Spec'
}

{ #category : #'instance creation' }
HiRulerLazyMorph class >> model: aModel key: anObject [ 
	^ self basicNew
		initializeWithModel: aModel 
		key: anObject;
		yourself
]

{ #category : #drawing }
HiRulerLazyMorph >> drawOn: aCanvas [
	aCanvas 
		translucentImage: self sourceForm
		at: self bounds origin 
		sourceRect: self sourceRectangle
]

{ #category : #initialization }
HiRulerLazyMorph >> initializeWithModel: aModel key: anObject [
	self initialize.
	
	model := aModel.
	key := anObject.
	
	self syncExtent.
]

{ #category : #accessing }
HiRulerLazyMorph >> sourceForm [
	^ model rulerForm
]

{ #category : #accessing }
HiRulerLazyMorph >> sourceRectangle [
	^ model rulerMorphBoundsFor: key
]

{ #category : #initialization }
HiRulerLazyMorph >> syncExtent [
	self extent: self sourceRectangle extent
]
