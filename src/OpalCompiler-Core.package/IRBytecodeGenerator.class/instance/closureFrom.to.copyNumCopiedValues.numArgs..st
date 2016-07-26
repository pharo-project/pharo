instructions
closureFrom: fromSeqId to: toSeqId copyNumCopiedValues: numCopied numArgs: numArgs2
	| distance from to |
	from := seqOrder at: fromSeqId.
	to := seqOrder at: toSeqId ifAbsent: [ ^ self ].	"not done yet"
	distance := (from + 1 to: to - 1) inject: 0 into: [ :size :i | size + (seqBytes at: (orderSeq at: i)) size ].
	encoder genPushClosureCopyNumCopiedValues: numCopied numArgs: numArgs2 jumpSize: distance. 