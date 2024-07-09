"
I am a FormSet that will save the forms I'm scaling for performance reasons. 

For example, if you have a background in Pharo it takes time to scale the beackground at each refresh of the UI.

Like this we can use FormSet when we want to save memory and CachedFormSet when we want to save time.
"
Class {
	#name : 'CachedFormSet',
	#superclass : 'FormSet',
	#category : 'Graphics-Display Objects-Forms',
	#package : 'Graphics-Display Objects',
	#tag : 'Forms'
}

{ #category : 'converting' }
CachedFormSet >> asFormWithExtent: formExtent [

	^ forms
		  detect: [ :form | form extent = formExtent ]
		  ifFound: [ :form | form asFormOfDepth: depth ]
		  ifNone: [
			  | newForm |
			  newForm := forms first scaledToExactSize: formExtent.
			  forms := forms copyWith: newForm.
			  newForm asFormOfDepth: depth ]
]
