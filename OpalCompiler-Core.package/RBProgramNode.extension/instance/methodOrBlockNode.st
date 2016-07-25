*opalcompiler-core
methodOrBlockNode
	^ parent ifNotNil: [ parent methodOrBlockNode ]