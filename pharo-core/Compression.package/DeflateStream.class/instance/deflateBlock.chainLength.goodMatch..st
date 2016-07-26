deflating
deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch
	"Continue deflating the receiver's collection from blockPosition to lastIndex.
	Note that lastIndex must be at least MaxMatch away from the end of collection"
	| here matchResult flushNeeded hereMatch hereLength newMatch newLength hasMatch |
	blockPosition > lastIndex ifTrue:[^false]. "Nothing to deflate"
	hasMatch := false.
	here := blockPosition.
	[here <= lastIndex] whileTrue:[
		hasMatch ifFalse:[
			"Find the first match"
			matchResult := self findMatch: here
								lastLength: MinMatch-1
								lastMatch: here
								chainLength: chainLength
								goodMatch: goodMatch.
			self insertStringAt: here. "update hash table"
			hereMatch := matchResult bitAnd: 16rFFFF.
			hereLength := matchResult bitShift: -16].

		"Look ahead if there is a better match at the next position"
		matchResult := self findMatch: here+1
							lastLength: hereLength
							lastMatch: hereMatch
							chainLength: chainLength
							goodMatch: goodMatch.
		newMatch := matchResult bitAnd: 16rFFFF.
		newLength := matchResult bitShift: -16.

		"Now check if the next match is better than the current one.
		If not, output the current match (provided that the current match
		is at least MinMatch long)"
		(hereLength >= newLength and:[hereLength >= MinMatch]) ifTrue:[
			[self validateMatchAt: here
							from: hereMatch to: hereMatch + hereLength - 1] assert.
			"Encode the current match"
			flushNeeded := self
				encodeMatch: hereLength
				distance: here - hereMatch.
			"Insert all strings up to the end of the current match.
			Note: The first string has already been inserted."
			1 to: hereLength-1 do:[:i| self insertStringAt: (here := here + 1)].
			hasMatch := false.
			here := here + 1.
		] ifFalse:[
			"Either the next match is better than the current one or we didn't
			have a good match after all (e.g., current match length < MinMatch).
			Output a single literal."
			flushNeeded := self encodeLiteral: (collection byteAt: (here + 1)).
			here := here + 1.
			(here <= lastIndex and:[flushNeeded not]) ifTrue:[
				"Cache the results for the next round"
				self insertStringAt: here.
				hasMatch := true.
				hereMatch := newMatch.
				hereLength := newLength].
		].
		flushNeeded ifTrue:[blockPosition := here. ^true].
	].
	blockPosition := here.
	^false