dropping/grabbing
dragTab: aSystemWindow event: anEvent in: aTabLabel
	"Drag a tab. Remove the window from the organiser and place in hand."
	
	self removeWindow: aSystemWindow.
	aSystemWindow position: anEvent targetPoint.
	anEvent hand grabMorph: aSystemWindow