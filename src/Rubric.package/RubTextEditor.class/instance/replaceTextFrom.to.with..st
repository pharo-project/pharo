accessing
replaceTextFrom: start to: stop with: aText
	self replace: (start to: stop - 1) with: aText and: []
