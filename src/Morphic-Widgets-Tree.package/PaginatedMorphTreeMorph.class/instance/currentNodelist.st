accessing
currentNodelist
	"The nodeList currently viewed (not the full node list if a pager is used)"
	^ self pager
		ifNil: [super nodeList]
		ifNotNil: [self pager currentNodelist]