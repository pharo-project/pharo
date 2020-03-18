Class {
	#name : #SliderTest,
	#superclass : #TestCase,
	#category : #'Morphic-Tests-Widgets'
}

{ #category : #tests }
SliderTest >> testDragSlider [
	
	| aSlider window| 
	
	"Create an Slider for the test"
	aSlider := Slider new.
aSlider extent: 240@16.
window:=aSlider openInWindow .

"Verify that now is not dragging"
self assert: aSlider dragging equals: false.

aSlider simulateSliderMouseDown.

"Verify that now is dragging"
self assert: aSlider dragging equals: true.

"Verify that the value is 0"
self assert: aSlider value equals: 0.

"Do the scrolling (mouse move)"
aSlider 
	simulateScrollAbsolute: aSlider bounds origin 
	to: aSlider bounds corner.
"Then release mouse button"
aSlider simulateSliderMouseUp.
"Verify that value is nor 1.0"
self assert: aSlider value equals: 1.
"Verify that now is not dragging"
self assert: aSlider dragging equals: false.
window close.

]
