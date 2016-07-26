typing/selecting keys
crWithIndent: aKeyboardEvent
	"Replace the current text selection with CR followed by as many tabs
	as on the current line (+/- bracket count) -- initiated by Shift-Return."
	| char s i tabCount crChars|
	self closeTypeIn.
	s := self string.
	i := self stopIndex.
	tabCount := 0.
	crChars := Array with: Character cr with: Character lf.
	[(i := i-1) > 0 and: [(crChars includes: (char := s at: i)) not]]
		whileTrue:  "Count tabs and brackets (but not a leading bracket)"
		[(char = Character tab and: [i < s size and: [(s at: i+1) ~= $[ ]]) ifTrue: [tabCount := tabCount + 1].
		char = $[ ifTrue: [tabCount := tabCount + 1].
		char = $] ifTrue: [tabCount := tabCount - 1]].
	 "Now inject CR with tabCount tabs"
	self addString: (String streamContents: [ :strm | strm crtab: tabCount ]).
	^ false