"
I check what a DiffMapMorph tells its owner when clicked.
"
Class {
	#name : 'DiffMapMorphTest',
	#superclass : 'TestCase',
	#instVars : [
		'map',
		'clickedPosition'
	],
	#category : 'Tool-Diff-Tests',
	#package : 'Tool-Diff-Tests'
}

{ #category : 'helpers' }
DiffMapMorphTest >> clickAtY: y [

	map mouseDown: (MouseButtonEvent new
		 setType: #mouseDown
		 position: 10 @ y
		 which: MouseButtonEvent redButton
		 buttons: MouseButtonEvent redButton
		 hand: nil
		 stamp: 0)
]

{ #category : 'helpers' }
DiffMapMorphTest >> clickableArea [
	"The map measures clicks inside its inner bounds, inset by 2"

	^ map innerBounds insetBy: 2
]

{ #category : 'running' }
DiffMapMorphTest >> setUp [

	super setUp.
	map := DiffMapMorph new.
	map bounds: (0 @ 0 extent: 20 @ 104).
	map whenClickedDo: [ :position | clickedPosition := position ]
]

{ #category : 'tests' }
DiffMapMorphTest >> testClickAtTheBottomAnswersOne [

	self clickAtY: self clickableArea bottom.
	self assert: clickedPosition equals: 1
]

{ #category : 'tests' }
DiffMapMorphTest >> testClickAtTheTopAnswersZero [

	self clickAtY: self clickableArea top.
	self assert: clickedPosition equals: 0
]

{ #category : 'tests' }
DiffMapMorphTest >> testClickInTheMiddleAnswersAHalf [

	self clickAtY: self clickableArea center y.
	self assert: clickedPosition equals: 0.5
]

{ #category : 'tests' }
DiffMapMorphTest >> testClickWithoutBlockDoesNothing [

	DiffMapMorph new mouseDown: (MouseButtonEvent new
		 setType: #mouseDown
		 position: 10 @ 10
		 which: MouseButtonEvent redButton
		 buttons: MouseButtonEvent redButton
		 hand: nil
		 stamp: 0)
]
