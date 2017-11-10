Class {
	#name : #RubScrolledTextMorphThemer,
	#superclass : #Object,
	#category : #Rubric-Editing-Widgets
}

{ #category : #border-styles }
RubScrolledTextMorphThemer >> disabledBorderStyleFor: aTextArea [
	^ self normalBorderStyleFor: aTextArea 
]

{ #category : #colors }
RubScrolledTextMorphThemer >> highlightBackgroundColorFor: aTextArea [

	^ Color veryLightGray alpha:0.01
]

{ #category : #border-styles }
RubScrolledTextMorphThemer >> highlightBorderStyleFor: aTextArea [
	^ BorderStyle inset 
		baseColor: Smalltalk ui theme selectionColor;
		width: 2
]

{ #category : #border-styles }
RubScrolledTextMorphThemer >> normalBorderStyleFor: aTextArea [
	^ BorderStyle 
		width: 0
		color: Color transparent
]
