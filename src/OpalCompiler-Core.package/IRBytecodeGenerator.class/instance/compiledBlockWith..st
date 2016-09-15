results
compiledBlockWith: trailer
	| lits header cb |
	lits := self literals allButLast "1 free only for compiledBlock".
	header := self spurVMHeader: lits size.
	cb := trailer createMethod: self bytecodes size class: CompiledBlock header: header.
	(WriteStream with: cb)
		position: cb initialPC - 1;
		nextPutAll: self bytecodes.
	lits withIndexDo: [:obj :i | cb literalAt: i put: obj].
	cb needsFrameSize: self stackFrameSize.
	self updateLiterals: cb.
	^ cb