"
! Morphic configurations

Morphic configurations will prepare your application to run in a Morphic backend. Tipically, you will not change much of what is already provided on a Pharo system, but there are several entry points you may want to extend/override: 

- *method:SpMorphicConfiguration#styleSheet*
"
Class {
	#name : #SpMorphicConfiguration,
	#superclass : #SpApplicationConfiguration,
	#instVars : [
		'styleSheet'
	],
	#category : #'Spec2-Morphic-Application'
}

{ #category : #styling }
SpMorphicConfiguration >> configure: anApplication [

	super configure: anApplication.
	anApplication styleSheet: self styleSheet
]

{ #category : #styling }
SpMorphicConfiguration >> styleSheet [
	"Define the default styleSheet to use in your application. You can override this and add your 
	 own application dependent styles (and you can compose them, see *SpStyle*).
	
	[[[language=Smalltalk
	^ SpStyle defaultStyleSheet copy
	]]]"
	
	^ styleSheet ifNil: [ styleSheet := SpStyle defaultStyleSheet copy ]
]
