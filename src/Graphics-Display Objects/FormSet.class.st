"
A class for representing images for which there can be more than one Form depicting the same image at different scales. A FormSet is created through #extent:depth:forms: with a given extent and depth and a SequenceableCollection containing at least one Form. There’s a convenience instance creation method #forms: that takes only the collection, the extent and depth are taken from the collection’s first Form. The convenience instance creation method #form: takes a single Form and uses the extent and depth of that Form.

A FormSet can be converted to a Form with a given extent through #asFormWithExtent: which either returns an element from the FormSet’s forms or converts one of them to the FormSet’s extent and depth. The convenience method #asForm uses the FormSet’s own extent, while the convenience method #asFormAtScale: uses the FormSet’s extent scaled by the given scale.
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

	^ forms
		  detect: [ :form | form extent = formExtent ]
		  ifFound: [ :form | form asFormOfDepth: depth ]
		  ifNone: [
			  | newForm |
			  newForm := forms first scaledToExactSize: formExtent.
			  forms := forms copyWith: newForm.
			  newForm asFormOfDepth: depth ]
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

{ #category : 'accessing' }
FormSet >> height [

	^ self extent y
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

{ #category : 'converting' }
FormSet >> mappableFormForScalingCanvas [

	forms detect: [ :form | form extent = extent ]
		ifFound: [ :form | form depth = depth ifTrue: [ ^ form ] ].
	^ nil
]

{ #category : 'copying' }
FormSet >> veryDeepCopyWith: deepCopier [
	"Return self.  I am immutable in the Morphic world.  Do not record me."
	^ self
]

{ #category : 'accessing' }
FormSet >> width [

	^ self extent x
]
