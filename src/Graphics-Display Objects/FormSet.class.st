"
A class for representing images for which there can be more than one Form depicting the same image at different scales.
"
Class {
	#name : 'FormSet',
	#superclass : 'Object',
	#instVars : [
		'extent',
		'depth',
		'forms'
	],
	#category : 'Graphics-Display Objects-Forms',
	#package : 'Graphics-Display Objects',
	#tag : 'Forms'
}

{ #category : 'instance creation' }
FormSet class >> extent: extent depth: depth forms: forms [

	^ self basicNew initializeWithExtent: extent depth: depth forms: forms
]

{ #category : 'instance creation' }
FormSet class >> form: form [

	^ self forms: { form }
]

{ #category : 'instance creation' }
FormSet class >> forms: forms [

	^ self extent: forms first extent depth: forms first depth forms: forms
]

{ #category : 'converting' }
FormSet >> asForm [

	^ self asFormAtScale: 1
]

{ #category : 'converting' }
FormSet >> asFormAtScale: scale [

	^ self asFormWithExtent: extent * scale
]

{ #category : 'converting' }
FormSet >> asFormWithExtent: formExtent [

	^ (forms detect: [ :form | form extent = formExtent ]
		ifFound: [ :form | form asFormOfDepth: depth ]
		ifNone: [ (forms first asFormOfDepth: depth) scaledToExactSize: formExtent ])
]

{ #category : 'accessing' }
FormSet >> depth [

	^ depth
]

{ #category : 'accessing' }
FormSet >> extent [

	^ extent
]

{ #category : 'accessing' }
FormSet >> forms [

	^ forms
]

{ #category : 'file in/out' }
FormSet >> hibernate [

	forms do: [ :form | form hibernate ]
]

{ #category : 'initialization' }
FormSet >> initializeWithExtent: initialExtent depth: initialDepth forms: initialForms [

	self initialize.
	extent := initialExtent.
	depth := initialDepth.
	forms := initialForms.
]

{ #category : 'copying' }
FormSet >> veryDeepCopyWith: deepCopier [
	"Return self.  I am immutable in the Morphic world.  Do not record me."
	^ self
]
