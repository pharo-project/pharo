"
self example1
"
Class {
	#name : #AthensCairoSurfaceExamples,
	#superclass : #AthensSurfaceExamples,
	#category : #'Athens-Examples-Demos'
}

{ #category : #examples }
AthensCairoSurfaceExamples class >> exampleDrawTextPath [
 
	| surf  font |
	font := LogicalFont familyName: 'Source Sans Pro' pointSize: 40.

	surf := self newSurface: 500@100.
	
	surf drawDuring: [:can |
		| f |
		"clear background"
		surf clear: Color white.
		
		f := CairoScaledFont fromFreetypeFont: font realFont. 
		"Set text position"
		can pathTransform translateX: 10 Y: (font getPreciseAscent)+10.
		"new path to draw the string"
		can newPath.
		
		f lock.
		"set font properties"
		can 
			setPathMatrix;
			setScaledFont: f.
		(Color red asAthensPaintOn: can)
			loadOnCairoCanvas: can.
		"creates a path text"
		can textPath: 'Text with border :V'.
		"It fills and preserve the last path"
		can fillPreserve.
		
		(can setStrokePaint: Color black)
			width: 2;
			dashes: #(10 1) offset: 0;
			"in order to use the path of cairo"
			prepareForDrawingOn: can.
		can stroke.
		f unlock.
	].
	
	surf asMorph openInWindow

]

{ #category : #examples }
AthensCairoSurfaceExamples class >> exampleInterop [

"
self exampleInterop

Test the interoperability with bitblt.
A cairo image surface bits are exposed to bitblt operations via surface plugin.
"
	| surf |
	
	surf := self newSurface: 100@100.

	surf asForm getCanvas fillRectangle: (0@0 corner: 50@50) color: Color red.
	surf markDirty. 
	
	surf drawDuring: [:can |
		can pathTransform loadIdentity.
		can pathTransform translateX: 30 Y: 30.
		can pathTransform rotateByDegrees: 35.
		can setPaint: (Color red).
		
		can setShape: (-20@ -20 corner: 20@ 20).
		2 timesRepeat: [ can draw. can setPaint: (Color green alpha:0.5)]
	].

	surf asMorph openInWindow
]

{ #category : #utilities }
AthensCairoSurfaceExamples class >> newSurface: extent [
	
	^ AthensCairoSurface extent: extent
]
