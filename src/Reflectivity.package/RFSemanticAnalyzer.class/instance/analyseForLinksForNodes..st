visiting
analyseForLinksForNodes: aNode
	| generator |
	aNode hasMetalink
		ifFalse: [ ^ self ].
	generator := HookGenerator node: aNode.
	self analyseForLinks: aNode generator: generator.
