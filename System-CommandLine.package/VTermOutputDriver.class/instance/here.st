navigating
here
	self flag: #todo. "use termcap here"
	self csiEscape.
	outStream nextPut: $E