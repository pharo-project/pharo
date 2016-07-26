instructions
if: bool goto: seqId1 otherwise: seqId2
	stack pop.
	stacks at: seqId1 put: (stack linkTo: (stacks at: seqId1 ifAbsentPut: [ nil ])).
	stacks at: seqId2 put: (stack linkTo: (stacks at: seqId2 ifAbsentPut: [ nil ])).
	self
		saveLastJump:
			(Message
				selector: #from:if:goto:otherwise:
				arguments:
					{currentSeqId.
					bool.
					seqId1.
					seqId2}).
	self
		from: currentSeqId
		if: bool
		goto: seqId1
		otherwise: seqId2