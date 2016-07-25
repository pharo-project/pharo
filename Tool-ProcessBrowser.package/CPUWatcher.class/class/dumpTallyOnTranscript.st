monitoring
dumpTallyOnTranscript
	self current ifNotNil: [
		self processBrowser dumpTallyOnTranscript: self current tally
	]