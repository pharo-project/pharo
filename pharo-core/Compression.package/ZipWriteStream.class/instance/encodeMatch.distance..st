encoding
encodeMatch: length distance: dist
	"Encode the given match of length length starting at dist bytes ahead"
	| literal distance |
	dist > 0 
		ifFalse:[^self error:'Distance must be positive'].
	length < MinMatch 
		ifTrue:[^self error:'Match length must be at least ', MinMatch printString].
	litCount := litCount + 1.
	matchCount := matchCount + 1.
	literals at: litCount put: length - MinMatch.
	distances at: litCount put: dist.
	literal := (MatchLengthCodes at: length - MinMatch + 1).
	literalFreq at: literal+1 put: (literalFreq at: literal+1) + 1.
	dist < 257
		ifTrue:[distance := DistanceCodes at: dist]
		ifFalse:[distance := DistanceCodes at: 257 + (dist - 1 bitShift: -7)].
	distanceFreq at: distance+1 put: (distanceFreq at: distance+1) + 1.
	^self shouldFlush