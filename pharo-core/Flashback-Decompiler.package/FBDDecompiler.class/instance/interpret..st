private
interpret: endPC
	[ self pc > endPC ]
		whileFalse: [ 
			(loopsArray at: self pc)
				ifNotNil: [ :ls | self decodeLoops: ls ]
				ifNil: [ instructionStream interpretNextInstructionFor: self ] ].
			