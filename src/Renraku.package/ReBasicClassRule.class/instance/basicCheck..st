running
basicCheck: aClass 
	self 
		deprecated: 'Please subclass directly from ReAbstractRule and override class method #checksClass to return true'
		on: 	'11 July 2016' 
		in: 'Pharo-6.0-60143-'.
	^ super basicCheck: aClass 