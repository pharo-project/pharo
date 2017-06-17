testing
ifNotReady: aBlock
	"use extra block so compiler can optimize"
	self isReady ifFalse: [ aBlock value ]