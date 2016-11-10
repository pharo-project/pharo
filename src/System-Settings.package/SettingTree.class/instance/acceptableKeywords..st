accessing
acceptableKeywords: keywords
	self collector
		filter: [ :prg | prg methodClass isMeta and: [ keywords includes: prg selector ] ].
	nodeList := nil.
	self collector reset