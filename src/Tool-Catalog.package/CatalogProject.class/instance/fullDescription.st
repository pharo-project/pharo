accessing
fullDescription
	^ String streamContents: [ :stream | 
		self description 
			ifNotNil: [ stream << self description  ].
		self contactInfo 
			ifNotNil: [ 
				stream cr; cr.
				stream << self contactInfo ] ]