private
from: fromSeqId if: bool goto: toSeqId otherwise: otherwiseSeqId

	| distance from to otherwise |
	from := seqOrder at: fromSeqId.
	to := seqOrder at: toSeqId ifAbsent: [^ self jump: 0 if: bool].  "not done yet"
	otherwise := seqOrder at: otherwiseSeqId ifAbsent: [^ self jump: 0 if: bool].  "not done yet"
	from < to ifFalse: [self error].
	from + 1 = otherwise ifFalse: [self error].
	distance := (from + 1 to: to - 1)
		inject: 0
		into: [:size :i | size + (seqBytes at: (orderSeq at: i)) size].
	self jump: distance if: bool.
