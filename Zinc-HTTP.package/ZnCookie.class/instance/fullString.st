converting
fullString
	^ String streamContents: [ :stream | 
		self writeOn: stream ]