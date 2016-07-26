temp vars
addTemp: aTempVariable withName: aString
	^ tempVars
		at: aString
		put: (aTempVariable
				name: aString;
				scope: self;
				yourself)