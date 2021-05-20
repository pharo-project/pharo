"
Text property controls how the ""input text"" fields (`SpTextInputFieldPresenter` hierarchy for now)
"
Class {
	#name : #SpTextStyle,
	#superclass : #SpPropertyStyle,
	#instVars : [
		'drawKeyboardFocus'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #operations }
SpTextStyle >> applyTo: aMorph [


	self drawKeyboardFocusVariable
		ifNotNil: [ :aVariable | aMorph drawKeyboardFocus: aVariable value ]
]

{ #category : #accessing }
SpTextStyle >> drawKeyboardFocus [

	^ self drawKeyboardFocusVariable value
]

{ #category : #accessing }
SpTextStyle >> drawKeyboardFocus: aBoolean [

	drawKeyboardFocus := aBoolean asStyleVariable
]

{ #category : #'accessing variables' }
SpTextStyle >> drawKeyboardFocusVariable [

	^ self toVariable: drawKeyboardFocus
]
