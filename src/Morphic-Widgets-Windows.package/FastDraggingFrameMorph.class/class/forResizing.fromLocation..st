instance creation
forResizing: aWindow fromLocation: aSideOrCorner
	| b  |
	b := self new.	
	b target: aWindow.
	b location: aSideOrCorner.
	^ b 