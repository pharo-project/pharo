"
A canvas which forwards drawing commands to sub-canvases.
"
Class {
	#name : #MultiCanvas,
	#superclass : #PluggableCanvas,
	#instVars : [
		'canvases',
		'extent',
		'depth'
	],
	#category : #'Graphics-Canvas'
}

{ #category : #accessing }
MultiCanvas >> addCanvas: aCanvas [
	canvases add: aCanvas
]

{ #category : #initialization }
MultiCanvas >> allocateForm: extentPoint [
	"Allocate a new form which is similar to the receiver and can be used for accelerated blts"
	^Form extent: extentPoint depth: self depth
]

{ #category : #private }
MultiCanvas >> apply: aCommand [

	self flag: #roundedRudeness.	
	"This rudeness is to help get rounded corners to work right on RemoteCanvases. Since the RemoteCanvas has no other way to read its bits, we are grabbing them from Display for now. To support this, we need to see that the Display is written before any RemoteCanvases"

	canvases do: [ :canvas | 
		(canvas isKindOf: FormCanvas) ifTrue: [aCommand value: canvas]
	].
	canvases do: [ :canvas | 
		(canvas isKindOf: FormCanvas) ifFalse: [aCommand value: canvas]
	].

]

{ #category : #accessing }
MultiCanvas >> clipRect [
	
	^super clipRect ifNil: [0@0 extent: 5000@5000]
]

{ #category : #accessing }
MultiCanvas >> contentsOfArea: aRectangle into: aForm [

	self apply: [ :c |
		(c isKindOf: FormCanvas) ifTrue: [
			c contentsOfArea: aRectangle into: aForm.
			^aForm
		].
	].
	self apply: [ :c |
		c contentsOfArea: aRectangle into: aForm.
		^aForm.
	].
	^aForm
]

{ #category : #accessing }
MultiCanvas >> depth [
	^depth
]

{ #category : #initialization }
MultiCanvas >> depth: newDepth [
	"set the extent to be used with this canvas"
	depth := newDepth.
]

{ #category : #accessing }
MultiCanvas >> extent [
	^extent
]

{ #category : #initialization }
MultiCanvas >> extent: newExtent [
	"set the extent to be used with this canvas"
	extent := newExtent.
]

{ #category : #initialization }
MultiCanvas >> initialize [
	super initialize.
	canvases := Set new.
	extent := 600@400.
	depth := 32. 
]

{ #category : #accessing }
MultiCanvas >> removeCanvas: aCanvas [
	canvases remove: aCanvas ifAbsent: []
]
