interactive error protocol
announceSelection: aString from: start to: stop
	self announcer announce: (
		GLMSelectedTextChanged new 
			start: start; 
			end: stop;
			selectedText: aString )