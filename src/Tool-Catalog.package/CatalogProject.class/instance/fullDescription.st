accessing
fullDescription
	^ String streamContents: [ :stream | 
		self description
		   ifNil: [ stream << '<No Description Provided>' ]
			ifNotNil: [ stream << self description  ].
		self contactInfo 
			ifNotNil: [ 
				stream cr; cr.
				stream << self contactInfo ] ]