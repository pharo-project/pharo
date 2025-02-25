"
I am a FormSet that will save the forms I'm scaling for performance reasons. 

For example, if you have a background in Pharo it takes time to scale the beackground at each refresh of the UI.

Like this we can use FormSet when we want to save memory and CachedFormSet when we want to save time.

The difference with my super class is that I'll save all requested scallings in a cache instead of recomputing all the time the scales.
"
Class {
	#name : 'CachedFormSet',
	#superclass : 'FormSet',
	#instVars : [
		'cache'
	],
	#category : 'Graphics-Display Objects-Forms',
	#package : 'Graphics-Display Objects',
	#tag : 'Forms'
}

{ #category : 'converting' }
CachedFormSet >> asFormWithExtent: formExtent [

	^ cache
		  detect: [ :form | form extent = formExtent ]
		  ifFound: [ :form | form asFormOfDepth: depth ]
		  ifNone: [
			  | newForm |
			  newForm := super asFormWithExtent: formExtent.
			  cache := cache copyWith: newForm.
			  newForm asFormOfDepth: depth ]
]

{ #category : 'private' }
CachedFormSet >> flushCache [

	cache := #(  )
]

{ #category : 'initialization' }
CachedFormSet >> initialize [

	super initialize.
	self flushCache
]
