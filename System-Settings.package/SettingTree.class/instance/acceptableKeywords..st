accessing
acceptableKeywords: keywords
	self collector filter: [:prg | prg methodClass isMeta and: [keywords includes: prg keyword]].
	nodeList := nil.
	self collector reset.
