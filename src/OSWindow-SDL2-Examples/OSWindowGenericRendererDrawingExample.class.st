"
I am a basic example to show how we can draw something.

To launch the example:
self new open
"
Class {
	#name : #OSWindowGenericRendererDrawingExample,
	#superclass : #OSWindowGenericRendererExample,
	#category : #'OSWindow-SDL2-Examples-Basic'
}

{ #category : #drawing }
OSWindowGenericRendererDrawingExample >> redraw [
	| seconds x y|
	seconds := Time millisecondClockValue * 0.001.
	
	x := 320 + (150 * (0.3 * Float twoPi * seconds) cos).
	y := 240 + (150 * (0.3 * Float twoPi * seconds) sin).
	
	renderer
		color: (Color gray: (seconds sin) *0.5 + 0.5);
		clear;
		color: Color blue;
		drawRectangle: (x @ y extent: 20 @ 20)
	
]
