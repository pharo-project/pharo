block closure support
isCreateFullBlock: compiledBlock code: code at: pc
	^self extensionsAt: pc in: code into:
		[:extA :extB :nExtBytes| 
			(code at: pc + nExtBytes) = 16rF9 and: [ 
				compiledBlock = (code literalAt: (code at: pc + nExtBytes + 1) + (extA bitShift: 8) + 1)] ]