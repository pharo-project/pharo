Class {
	#name : #SpMorphicRichTextAdapter,
	#superclass : #SpMorphicTextAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #private }
SpMorphicRichTextAdapter >> applyTextStyle [
	
	self 
		setText: self presenter text
		to: self widget
]

{ #category : #factory }
SpMorphicRichTextAdapter >> buildWidget [
	| newWidget |
	
	newWidget := super buildWidget.
	self presenter whenTextStylerChangedDo: [ self applyTextStyle ].
	
	^ newWidget
]

{ #category : #private }
SpMorphicRichTextAdapter >> setText: text to: aWidget [

	super 
		setText: (self presenter textStyler applyStyleTo: text) 
		to: aWidget
]
