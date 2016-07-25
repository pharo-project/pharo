opening
openAbove: aRectangle
	"aRectangle must be in global coordinates"
	^ self asBrickedMorph
		vShrinkWrap;
		hShrinkWrap;
		bounds: ((aRectangle origin - (0@self contentBrick height) + (0@self themer popupThemer borderWidth)) extent: self extent);
		openInWorld
	