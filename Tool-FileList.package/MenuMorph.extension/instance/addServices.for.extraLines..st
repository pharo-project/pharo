*Tool-FileList
addServices: services for: served extraLines: linesArray

	services withIndexDo: [:service :i |
		self 
		addService: service for: served;
		balloonTextForLastItem: service description.
		(service usingLineAfter or: [linesArray includes: i]) 
			ifTrue: [ self addLine ]
	].
