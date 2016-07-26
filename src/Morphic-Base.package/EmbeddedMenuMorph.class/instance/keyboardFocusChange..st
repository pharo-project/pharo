keyboard control
keyboardFocusChange: aBoolean
	"Nasty hack for scrolling upon keyboard focus."

	super keyboardFocusChange: aBoolean.
	aBoolean
		ifTrue: [(self ownerThatIsA: GeneralScrollPane) ifNotNil: [:sp |
					sp scrollToShow: self bounds]]
		ifFalse: [self selectItem: nil event: nil]