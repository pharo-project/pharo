accessing
nodeModel: aNodeBuilder 
	self 
		deprecated: 'Use nodeBuilder: instead'
		on: 	'17 March 2016' 
		in: 'Pharo-5.0-50571-'.
	^ self nodeBuilder: aNodeBuilder 