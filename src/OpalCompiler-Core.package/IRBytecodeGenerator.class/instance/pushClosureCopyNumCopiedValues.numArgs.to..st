instructions
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs2 to: toSeqId
	| blockSeqId |
	blockSeqId := self newDummySeqId.
	stack pop: numCopied.
	stacks at: blockSeqId put: ((IRClosureStackCount new numMethodTempVars:(numberOfTemps)) startAt: (numArgs2+numCopied)).
	stack push.
	stacks at: toSeqId put: (stack linkTo: (stacks at: toSeqId ifAbsentPut: [ nil ])).
	self
		saveLastJump:
			(Message
				selector: #closureFrom:to:copyNumCopiedValues:numArgs:
				arguments:
					{currentSeqId.
					toSeqId.
					numCopied.
					numArgs2}).
	self
		closureFrom: currentSeqId
		to: toSeqId
		copyNumCopiedValues: numCopied
		numArgs: numArgs2.
	self label: blockSeqId