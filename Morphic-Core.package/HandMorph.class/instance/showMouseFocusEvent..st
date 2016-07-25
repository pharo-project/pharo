events-processing
showMouseFocusEvent: evt

	ShowEvents == true ifTrue: [
		| ofs| 
		ofs := (owner hands indexOf: self) - 1 * 60.
		self mouseFocus printString displayAt: (0@ofs) + (0@15)].
		