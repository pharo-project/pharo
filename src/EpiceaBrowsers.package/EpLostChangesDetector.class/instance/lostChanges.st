accessing
lostChanges
	
	self browserIfLostChanges: [:browser | 
		^ browser viewEntryItems collect: #entry ].
	^ #()