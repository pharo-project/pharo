comparing
startingAt: keyStart match: text startingAt: textStart
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."
	
	| anyMatch matchStart matchEnd i matchStr j ii jj |
	i := keyStart.
	j := textStart.
	
	"Process consecutive *s and #s at the beginning."
	anyMatch := false.
	[ i <= self size and: [
		(self at: i)
			caseOf: {
				[ $* ] -> [ 
					anyMatch := true.
					i := i + 1.
					true ].
				[ $# ] -> [
					i := i + 1.
					j := j + 1.
					true ] }
			otherwise: [ false ] ] ] whileTrue.
	i > self size ifTrue: [
		^j - 1 = text size or: [ "We reached the end by matching the character with a #."
			anyMatch and: [ j <= text size ] "Or there was a * before the end." ] ].
	matchStart := i.

	"Now determine the match string"
	matchEnd := self size.
	(ii := self indexOf: $* startingAt: matchStart) > 0 ifTrue: [ matchEnd := ii-1 ].
	(ii := self indexOf: $# startingAt: matchStart) > 0 ifTrue: [ matchEnd := matchEnd min: ii-1 ].
	matchStr := self copyFrom: matchStart to: matchEnd.

	"Now look for the match string"
	[jj := text findString: matchStr startingAt: j caseSensitive: false.
	anyMatch ifTrue: [jj > 0] ifFalse: [jj = j]]
		whileTrue:
		["Found matchStr at jj.  See if the rest matches..."
		(self startingAt: matchEnd+1 match: text startingAt: jj + matchStr size) ifTrue:
			[^ true "the rest matches -- success"].
		"The rest did not match."
		anyMatch ifFalse: [^ false].
		"Preceded by * -- try for a later match"
		j := j+1].
	^ false "Failed to find the match string"