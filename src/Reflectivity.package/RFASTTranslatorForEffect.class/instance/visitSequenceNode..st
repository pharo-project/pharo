visitor-double dispatching
visitSequenceNode: aSequenceNode
	self emitMetaLinkBefore: aSequenceNode.
	self emitPrepareLinkAfter: aSequenceNode.
	
	aSequenceNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aSequenceNode ]
		ifFalse: [ aSequenceNode statements do: [:each | self visitNode: each]. ].
	self emitMetaLinkAfter: aSequenceNode.
	(aSequenceNode hasMetalinkAfter or: [ aSequenceNode hasMetalinkInstead ]) ifTrue: [aSequenceNode parent isMethod 
			ifTrue: [methodBuilder returnTop]
			ifFalse: [methodBuilder popTop]]