drawing
drawCheckOn: aCanvas
	| topLeft icon |
	
	topLeft := self computeCheckTopLeft.
	icon := self retrieveCheckIcon.
	
	aCanvas
		drawImage: icon
		at: topLeft