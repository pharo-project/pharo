visiting
visitMethodNode: aMethodNode
	aMethodNode removeProperty: #scope ifAbsent: [ ].
	super visitMethodNode: aMethodNode