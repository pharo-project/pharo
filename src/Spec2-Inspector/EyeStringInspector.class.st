"
EyeStringInspector is is the EyeInspector for Strings.
Right now, the only difference is that the list of elements for the tree is overridden to be empty.

"
Class {
	#name : #EyeStringInspector,
	#superclass : #EyeCollectionInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
EyeStringInspector >> elements [
	^ #()
]
