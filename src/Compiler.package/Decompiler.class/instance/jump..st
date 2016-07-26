instruction decoding
jump: dist
	| blockBody destPc nextPC |
	destPc := pc + dist.
	(lastJumpIfPcStack isEmpty or: [dist < 0 and: [destPc > lastJumpIfPcStack last]])
		ifTrue:
			["Rule: aBackward jump not crossing a Bfp/Btp must be a repeat"
			nextPC := pc.
			pc := destPc.
			blockBody := self statementsTo: lastPc.
			blockBody size timesRepeat: [statements removeLast].
			pc := nextPC.
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor
								codeSelector: #repeat
								code: #macro)
					arguments: #()).
			]
		ifFalse:
			[exit := destPc.
			lastJumpPc := lastPc]