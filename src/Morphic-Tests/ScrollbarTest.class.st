Class {
	#name : #ScrollbarTest,
	#superclass : #SliderTest,
	#category : #'Morphic-Tests-Widgets'
}

{ #category : #tests }
ScrollbarTest >> createSlider [

	^ ScrollBar new
		extent: 240 @ 8;
		yourself
]

{ #category : #tests }
ScrollbarTest >> setUp [

	super setUp.
	"Create a ScrollBar for the test"
	aSlider := ScrollBar new.
	aSlider extent: 240 @ 8.
	window := aSlider openInWindow.
]
