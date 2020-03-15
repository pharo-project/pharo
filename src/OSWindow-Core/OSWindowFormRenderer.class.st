"
I provide a way of delivering form's bits to window's contents.
To obtain my instance, first one create a valid OSWindow and then use #newFormRenderer: .

Then you can render on form, and form's contents will serve as medium which will be displayed inside a window.
"
Class {
	#name : #OSWindowFormRenderer,
	#superclass : #OSWindowRenderer,
	#instVars : [
		'form'
	],
	#category : #'OSWindow-Core-Renderer'
}

{ #category : #'morphic integration' }
OSWindowFormRenderer >> deferUpdatesWhile: aBlock [

	aBlock value
]

{ #category : #accessing }
OSWindowFormRenderer >> drawDuring: aBlock [

	aBlock value: self getCanvas
]

{ #category : #'morphic integration' }
OSWindowFormRenderer >> forceDisplayUpdate [
]

{ #category : #accessing }
OSWindowFormRenderer >> form [
	
	^ form
]

{ #category : #accessing }
OSWindowFormRenderer >> form: anObject [
	
	form := anObject
]

{ #category : #accessing }
OSWindowFormRenderer >> getCanvas [
	^ form getCanvas
]

{ #category : #size }
OSWindowFormRenderer >> newExtent: newExtent [
	form setExtent: newExtent depth: form depth
]

{ #category : #'morphic integration' }
OSWindowFormRenderer >> updateAreas: allDamage immediate: forceToScreen [
	"Force all the damage rects to the screen."
	allDamage do: [ :r| self updateRectangle: r ].
]
