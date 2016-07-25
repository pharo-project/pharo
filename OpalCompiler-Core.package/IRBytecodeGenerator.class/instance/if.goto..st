instructions
if: bool goto: seqId
	| otherwiseSeqId |
	otherwiseSeqId := self newDummySeqId.
	self if: bool goto: seqId otherwise: otherwiseSeqId.
	self label: otherwiseSeqId