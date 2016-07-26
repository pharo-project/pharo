initialization
initialize

	super initialize.

	actionHolder := [] asValueHolder.
	displayBlock := [ :e | e printString ].
	icon := nil