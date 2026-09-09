"
I convert IconStylerDecoration instances into native back-end decorations.

Subclasses implement addDecoration: and clearDecorations for specific backends (Rubric, Spec).
"
Class {
	#name : 'IconStylerRenderer',
	#superclass : 'Object',
	#instVars : [
		'textModel'
	],
	#category : 'Reflectivity-Tools-Styler',
	#package : 'Reflectivity-Tools',
	#tag : 'Styler'
}

{ #category : 'rendering' }
IconStylerRenderer >> addDecoration: anIconStylerDecoration [

	self subclassResponsibility
]

{ #category : 'rendering' }
IconStylerRenderer >> clearDecorations [

	self subclassResponsibility
]

{ #category : 'accessing' }
IconStylerRenderer >> textModel [

	^ textModel
]

{ #category : 'accessing' }
IconStylerRenderer >> textModel: anObject [

	textModel := anObject
]
