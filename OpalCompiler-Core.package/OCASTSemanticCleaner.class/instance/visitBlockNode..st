visiting
visitBlockNode: aBlockNode
	aBlockNode removeProperty: #scope ifAbsent: [ ].
	super visitBlockNode: aBlockNode.
	