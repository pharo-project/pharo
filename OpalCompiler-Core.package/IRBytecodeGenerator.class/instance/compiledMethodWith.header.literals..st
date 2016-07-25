results
compiledMethodWith: trailer header: header literals: lits
	| cm |
	cm := trailer createMethod: self bytecodes size class: CompiledMethod header: header.
	(WriteStream with: cm)
		position: cm initialPC - 1;
		nextPutAll: self bytecodes.
	lits withIndexDo: [:obj :i | cm literalAt: i put: obj].
	cm needsFrameSize: self stackFrameSize.
	
	self addProperties: cm.
	^ cm