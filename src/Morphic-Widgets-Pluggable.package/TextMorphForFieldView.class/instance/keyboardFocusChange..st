changed
keyboardFocusChange: aBoolean 
	"If we are losing focus and have acceptOnFocusChange then accept."
	aBoolean
		ifTrue: [self editView selectAll]
		ifFalse: [self editView selectFrom: 1 to: 0].
	super keyboardFocusChange: aBoolean