"
My subclasses know how to render a Hiedra visualization (HiRuler) using Athens.

"
Class {
	#name : #HiAbstractRenderer,
	#superclass : #Object,
	#instVars : [
		'ruler',
		'rowsInterval'
	],
	#category : #'Hiedra-UI'
}

{ #category : #accessing }
HiAbstractRenderer >> cellWidth [
	^ self subclassResponsibility
]

{ #category : #accessing }
HiAbstractRenderer >> formExtent [
	"Answer the pixel extent for the form to be rendered (as a Point)."

	^ self formWidth @ self formHeight
]

{ #category : #accessing }
HiAbstractRenderer >> formHeight [
	"Answer the height in pixels for the form to be rendered."

	^ rowsInterval size * self rowHeight
]

{ #category : #accessing }
HiAbstractRenderer >> formWidth [
	"Answer the width in pixels for the form to be rendered."

	"Note: The reason of numberOfColumns + 2 is the left and right margin."
	^ (ruler numberOfColumns + 2) * self cellWidth
]

{ #category : #API }
HiAbstractRenderer >> newForm [
	"Answer a new form, rendered with the current ruler and rowInterval."
	
	self subclassResponsibility
]

{ #category : #accessing }
HiAbstractRenderer >> rowHeight [
	^ self subclassResponsibility
]

{ #category : #accessing }
HiAbstractRenderer >> rowsInterval [
	^ rowsInterval
]

{ #category : #accessing }
HiAbstractRenderer >> rowsInterval: anObject [
	rowsInterval := anObject
]

{ #category : #accessing }
HiAbstractRenderer >> ruler [
	^ ruler
]

{ #category : #accessing }
HiAbstractRenderer >> ruler: aHiRuler [
	ruler := aHiRuler

]
