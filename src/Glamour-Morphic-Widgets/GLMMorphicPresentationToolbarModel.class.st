Class {
	#name : #GLMMorphicPresentationToolbarModel,
	#superclass : #Object,
	#instVars : [
		'renderer',
		'presentation'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #accessing }
GLMMorphicPresentationToolbarModel >> presentation [
	^ presentation
]

{ #category : #accessing }
GLMMorphicPresentationToolbarModel >> presentation: anObject [
	presentation := anObject
]

{ #category : #accessing }
GLMMorphicPresentationToolbarModel >> renderer [
	^ renderer
]

{ #category : #accessing }
GLMMorphicPresentationToolbarModel >> renderer: anObject [
	renderer := anObject
]
