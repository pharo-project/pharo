private
processAfter: anExample
	anExample == self example ifFalse: [ ^ self ].
	processed reverseDo: [ :processedResult |
		self processAfter: processedResult example withResult: processedResult returnValue ]