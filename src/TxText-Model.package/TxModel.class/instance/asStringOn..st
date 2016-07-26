conversion
asStringOn: aStream
	self spansDo: [ :span | span asStringOn: aStream ].