private
from: fromSeqId goto: toSeqId

	| distance from to |
	from := seqOrder at: fromSeqId.
	to := seqOrder at: toSeqId ifAbsent: [^ self].
	from + 1 = to ifTrue: [^ self].  "fall through, no jump needed"

	from < to ifTrue: [ "jump forward"
		distance := (from + 1 to: to - 1) inject: 0 into: [:size :i | 
				size + (seqBytes at: (orderSeq at: i)) size].
		self jumpForward: distance.
	] ifFalse: [ "jump backward"
		distance := ((to to: from - 1) inject: 0 into: [:size :i |
				size + (seqBytes at: (orderSeq at: i)) size])
			+ bytes size.
		self jumpBackward: distance.
	].
