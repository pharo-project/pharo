dct
idctMCU
	| comp fp ci |
	fp := self useFloatingPoint.
	1 
		to: mcuMembership size
		do: 
			[ :i | 
			ci := mcuMembership at: i.
			comp := currentComponents at: ci.
			fp 
				ifTrue: 
					[ self 
						idctBlockFloat: (mcuSampleBuffer at: i)
						component: comp ]
				ifFalse: 
					[ self 
						primIdctInt: (mcuSampleBuffer at: i)
						qt: (qTable at: comp qTableIndex) ] ]