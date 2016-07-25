accessing
nodeList
	^ nodeList ifNil: [nodeList := self getList]
