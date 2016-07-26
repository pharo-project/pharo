typing/selecting keys
crWithIndent: aKeyboardEvent
	"Replace the current text selection with CR followed by as many tabs
	as on the current line (+/- bracket count) -- initiated by Shift-Return."
	| char s i tabCount  b |
	self closeTypeIn.
	b := self paragraph characterBlockAtPoint: aKeyboardEvent cursorPoint.
	s := self string.
	i := self stopIndex.
	tabCount := 0.
	[(i := i-1) > 0 and: [(char := s at: i) ~= Character cr]]
		whileTrue:  "Count tabs and brackets (but not a leading bracket)"
		[(char = Character tab and: [i < s size and: [(s at: i+1) ~= $[ ]]) ifTrue: [tabCount := tabCount + 1].
		char = $[ ifTrue: [tabCount := tabCount + 1].
		char = $] ifTrue: [tabCount := tabCount - 1]].
	 "Now inject CR with tabCount tabs"
	self addString: (String streamContents: [ :strm | strm crtab: tabCount ]).
	^ false