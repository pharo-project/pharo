tests
testProtocolRemovalOfUnclassifiedProtocol
	"This protocol is not relevant then it's not logged."

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: Protocol unclassified.
	aClass removeProtocol: Protocol unclassified.

	self assert: (self countLogEventsWith: EpProtocolRemoval) equals: 0