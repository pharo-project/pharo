as yet unclassified
textEmphasisCode
	"Answer the current bitmask for the text emphasis."

	^(((self isBold ifTrue: [1] ifFalse: [0]) bitOr:
		(self isItalic ifTrue: [2] ifFalse: [0])) bitOr:
		(self isUnderlined ifTrue: [4] ifFalse: [0])) bitOr:
		(self isStruckOut ifTrue: [16] ifFalse: [0])