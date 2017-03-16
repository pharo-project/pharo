accessing
lostChanges
	
	self browserIfLostChanges: [:browser | 
		^ browser filteredEntries ].
	^ #()