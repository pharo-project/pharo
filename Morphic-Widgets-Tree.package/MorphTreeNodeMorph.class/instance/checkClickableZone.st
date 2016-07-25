mouse events
checkClickableZone
	| topLeft icon |
	
	topLeft := self computeCheckTopLeft.
	icon := self retrieveCheckIcon.
	
	^ topLeft corner: icon extent + topLeft