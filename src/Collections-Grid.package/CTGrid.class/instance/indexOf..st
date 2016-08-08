access
indexOf: anElement
	"If there are integers r, c such that (self at: r at: c) = anElement, answer some such r@c, otherwise answer 0@0. The order in which the receiver are searched is UNSPECIFIED except that it is the same as the order used by #indexOf:ifAbsent: and #readStream."

	^self indexOf: anElement ifAbsent: [0@0]
