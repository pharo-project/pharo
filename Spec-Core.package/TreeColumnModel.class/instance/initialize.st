initialization
initialize

	super initialize.

	container := nil asValueHolder.
	displayBlock := [ :node | node printString ] asValueHolder.
	initialWidth := 100 asValueHolder.
	resizable := true asValueHolder.
	headerLabel := nil asValueHolder.
	headerFont := StandardFonts defaultFont asValueHolder.
	headerIcon := nil asValueHolder.
	headerAction := nil asValueHolder.
	
	self registerEvents