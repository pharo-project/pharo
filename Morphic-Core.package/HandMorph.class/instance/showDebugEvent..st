events-processing
showDebugEvent: evt

	ShowEvents == true ifTrue: [
		| ofs| 
		Display fill: (0@0 extent: 500@120) rule: Form over fillColor: Color white.
		ofs := (owner hands indexOf: self) - 1 * 60.
		evt printString displayAt: (0@ofs) + (evt isKeyboard ifTrue: [0@30] ifFalse: [0@0]).
		self keyboardFocus printString displayAt: (0@ofs)+(0@45).
		].