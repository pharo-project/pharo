Class {
	#name : #ScrollbarTest,
	#superclass : #TestCase,
	#category : #'Morphic-Tests-Widgets'
}

{ #category : #tests }
ScrollbarTest >> testDragScrollbar [
	| aScrollBar window |
	"Create a ScrollBar for the test"
	aScrollBar := Slider new.
	aScrollBar extent: 240 @ 8.
	window := aScrollBar openInWindow.

	"Verify that now is not dragging"
	self assert: aScrollBar dragging equals: false.
	aScrollBar simulateSliderMouseDown.

	"Verify that now is dragging"
	self assert: aScrollBar dragging equals: true.

	"Verify that the value is 0"
	self assert: aScrollBar value equals: 0.

	"Do the scrolling (mouse move)"
	aScrollBar
		simulateScrollAbsolute: aScrollBar bounds origin
		to: aScrollBar bounds corner.
	"Then release mouse button"
	aScrollBar simulateSliderMouseUp.
	"Verify that value is nor 1.0"
	self assert: aScrollBar value equals: 1.
	"Verify that now is not dragging"
	self assert: aScrollBar dragging equals: false.
	window close
]

{ #category : #tests }
ScrollbarTest >> testNoDragScrollbar [
	| aScrollBar window |
	"Create an Scrollbar for the test"
	aScrollBar := Slider new.
	aScrollBar extent: 240 @ 8.
	window := aScrollBar openInWindow.

	"Verify that now is not dragging"
	self assert: aScrollBar dragging equals: false.
	aScrollBar simulateSliderMouseDown.

	"Verify that now is dragging"
	self assert: aScrollBar dragging equals: true.

	"Then release mouse button"
	aScrollBar simulateSliderMouseUp.

	"Verify that now is not dragging"
	self assert: aScrollBar dragging equals: false.

	"Verify that the value is 0"
	self assert: aScrollBar value equals: 0.

	"Do the scrolling (mouse move)"
	aScrollBar
		simulateScrollAbsolute: aScrollBar bounds origin
		to: aScrollBar bounds corner.

	"Verify that value is (still) 0"
	self assert: aScrollBar value equals: 0.
	"Verify that is not dragging"
	self assert: aScrollBar dragging equals: false.
	window close
]
