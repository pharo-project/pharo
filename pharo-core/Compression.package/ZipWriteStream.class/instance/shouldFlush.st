encoding
shouldFlush
	"Check if we should flush the current block.
	Flushing can be useful if the input characteristics change."
	| nLits |
	litCount = literals size ifTrue:[^true]. "We *must* flush"
	(litCount bitAnd: 16rFFF) = 0 ifFalse:[^false]. "Only check every N kbytes"
	matchCount * 10 <= litCount ifTrue:[
		"This is basically random data. 
		There is no need to flush early since the overhead
		for encoding the trees will add to the overall size"
		^false].
	"Try to adapt to the input data.
	We flush if the ratio between matches and literals
	changes beyound a certain threshold"
	nLits := litCount - matchCount.
	nLits <= matchCount ifTrue:[^false]. "whow! so many matches"
	^nLits * 4 <= matchCount