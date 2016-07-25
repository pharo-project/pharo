*System-Changes
parseLangTagFor: aString

	| string peek runsRaw pos |
	string := aString.
	"Test for ]lang[ tag"
	pos := self position.
	peek := self skipSeparatorsAndPeekNext.
	peek = $] ifFalse: [self position: pos. ^ string].  "no tag"
	(self upTo: $[) = ']lang' ifTrue: [
		runsRaw := self basicNextChunk.
		string := self decodeString: aString andRuns: runsRaw
	] ifFalse: [
		self position: pos
	].
	^ string.
