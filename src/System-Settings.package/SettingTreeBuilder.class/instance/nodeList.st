accessing
nodeList
	^ nodeList ifNil: [nodeList := OrderedCollection new]
