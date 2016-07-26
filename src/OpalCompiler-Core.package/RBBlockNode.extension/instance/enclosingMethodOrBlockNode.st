*opalcompiler-core
enclosingMethodOrBlockNode
	^ parent ifNotNil: [ parent methodOrBlockNode ]