Class {
	#name : #GLMTextModel,
	#superclass : #GLMMorphicModel,
	#instVars : [
		'text'
	],
	#category : 'Glamour-Morphic-Widgets'
}

{ #category : #callbacks }
GLMTextModel >> accept: aText notifying: anObject [ 
	aText asString trimBoth ifEmpty: [^self].
	self text: aText.
	self changed: #text
]

{ #category : #accessing }
GLMTextModel >> changedText: aTextOrString [
	text := aTextOrString.
	self changed: #text
]

{ #category : #callbacks }
GLMTextModel >> keystroke: anEvent from: aMorph [
	anEvent anyModifierKeyPressed ifTrue: [ ^ super keystroke: anEvent from: aMorph ].
	self glamourPresentation text: self text.
	^ false
]

{ #category : #callbacks }
GLMTextModel >> menu: aMorphMenu shifted: b [
	^ self menu: aMorphMenu
]

{ #category : #callbacks }
GLMTextModel >> selectSilently: anIndex [
	self changed: #selection
]

{ #category : #callbacks }
GLMTextModel >> selection [
	^ self glamourPresentation selectionInterval ifNil: [1 to: 0]
]

{ #category : #callbacks }
GLMTextModel >> text [
	^text
]

{ #category : #accessing }
GLMTextModel >> text: aTextOrString [
	text := aTextOrString.
	self glamourPresentation ifNotNil: [ :presentation | presentation text: text ]
]
