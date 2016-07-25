opening
openBelow: aRectangle
	"aRectangle must be in global coordinates"
	^ self asBrickedMorph
		vShrinkWrap;
		hShrinkWrap;
		bounds: (aRectangle origin extent: (self extent + (0@self themer popupThemer borderWidth)));
		openInWorld
	