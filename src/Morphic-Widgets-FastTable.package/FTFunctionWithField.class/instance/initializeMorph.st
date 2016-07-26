initialization
initializeMorph
	field := RubScrolledTextModel new newTextField
		hResizing: #spaceFill;
		autoAccept: true;
		withGhostText: self ghostText;
		yourself.
	field model announcer subscribe: RubTextUpdatedInModel send: #textUpdated: to: self