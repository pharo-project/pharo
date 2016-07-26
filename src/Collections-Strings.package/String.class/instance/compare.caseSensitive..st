comparing
compare: aString caseSensitive: aBool
	"Answer a comparison code telling how the receiver sorts relative to aString:
		1 - before
		2 - equal
		3 - after.
	"
	| map |
	map := aBool ifTrue:[CaseSensitiveOrder] ifFalse:[CaseInsensitiveOrder].
	^self compare: self with: aString collated: map