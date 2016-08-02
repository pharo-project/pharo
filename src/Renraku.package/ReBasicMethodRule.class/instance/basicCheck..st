running
basicCheck: aMethod 
	self 
		deprecated: 'Please subclass directly from ReAbstractRule and override class method #checksMethod to return true'
		on: 	'11 July 2016' 
		in: 'Pharo-6.0-60143-'.
	^ super basicCheck: aMethod 