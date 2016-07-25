converting
nameValueString
	^ String streamContents: [ :stream | 
		self writeNameAndValueOn: stream ]