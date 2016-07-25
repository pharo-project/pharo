brick-layout-events
onLayouted
	super onLayouted.
	
	self isChecked ifTrue: [ self stepModel spotter announce: GTSpotterCandidateSelectedMoved new ]