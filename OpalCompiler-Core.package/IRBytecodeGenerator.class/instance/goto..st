instructions
goto: seqId
	stacks at: seqId put: (stack linkTo: (stacks at: seqId ifAbsentPut: [ nil ])).
	self
		saveLastJump:
			(Message
				selector: #from:goto:
				arguments:
					{currentSeqId.
					seqId}).
	self from: currentSeqId goto: seqId