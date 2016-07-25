deflating
findMatch: here lastLength: lastLength lastMatch: lastMatch chainLength: maxChainLength goodMatch: goodMatch
	"Find the longest match for the string starting at here.
	If there is no match longer than lastLength return lastMatch/lastLength.
	Traverse at most maxChainLength entries in the hash table.
	Stop if a match of at least goodMatch size has been found."
	| matchResult matchPos distance chainLength limit bestLength length |
	"Compute the default match result"
	matchResult := (lastLength bitShift: 16) bitOr: lastMatch.

	"There is no way to find a better match than MaxMatch"
	lastLength >= MaxMatch ifTrue:[^matchResult].

	"Start position for searches"
	matchPos := hashHead at: (self updateHashAt: here + MinMatch) + 1.

	"Compute the distance to the (possible) match"
	distance := here - matchPos.

	"Note: It is required that 0 < distance < MaxDistance"
	(distance > 0 and:[distance < MaxDistance]) ifFalse:[^matchResult].

	chainLength := maxChainLength.	"Max. nr of match chain to search"
	here > MaxDistance	"Limit for matches that are too old"
		ifTrue:[limit := here - MaxDistance]
		ifFalse:[limit := 0].

	"Best match length so far (current match must be larger to take effect)"
	bestLength := lastLength.

	["Compare the current string with the string at match position"
	length := self compare: here with: matchPos min: bestLength.
	"Truncate accidental matches beyound stream position"
	(here + length > position) ifTrue:[length := position - here].
	"Ignore very small matches if they are too far away"
	(length = MinMatch and:[(here - matchPos) > (MaxDistance // 4)])
		ifTrue:[length := MinMatch - 1].
	length > bestLength ifTrue:["We have a new (better) match than before"
		"Compute the new match result"
		matchResult := (length bitShift: 16) bitOr: matchPos.
		bestLength := length.
		"There is no way to find a better match than MaxMatch"
		bestLength >= MaxMatch ifTrue:[^matchResult].
		"But we may have a good, fast match"
		bestLength > goodMatch ifTrue:[^matchResult].
	].
	(chainLength := chainLength - 1) > 0] whileTrue:[
		"Compare with previous entry in hash chain"
		matchPos := hashTail at: (matchPos bitAnd: WindowMask) + 1.
		matchPos <= limit ifTrue:[^matchResult]. "Match position is too old"
	].
	^matchResult