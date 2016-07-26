event handling
mouseWheel: event


	"Handle a mouseWheel event."
	
	| center zoom  |
	
	center := transform inverseTransform: (event cursorPoint - bounds origin).

	zoom := 1.
	
	event isUp ifTrue: [ zoom := 1.25 ].
	event isDown ifTrue: [ zoom := 1/1.25 ].
	
	
	(self inState: #zooming) ifTrue: [   
		self updateZoom: zoom cursor: event cursorPoint.
	] ifFalse: [
		self startZooming: zoom center: center.
	]  