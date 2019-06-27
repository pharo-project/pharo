"
EyeStringInspector is is the EyeInspector for Strings.
Right now, the only difference is that the list of elements for the tree is overridden to be empty.

"
Class {
	#name : #SpEyeStringInspector,
	#superclass : #SpEyeCollectionInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
SpEyeStringInspector >> elements [
	^ #()
]
