temp vars
addCopyingTemp: aTempVar
	^ copiedVars add: (OCCopyingTempVariable new
			originalVar: aTempVar originalVar;
			name: aTempVar name;
			escaping: aTempVar escaping;
			usage: aTempVar usage;
			scope: self;
			yourself)