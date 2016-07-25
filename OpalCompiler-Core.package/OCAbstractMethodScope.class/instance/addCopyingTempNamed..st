temp vars
addCopyingTempNamed: name
	^ copiedVars add: (OCCopyingTempVariable new
			name: name;
			scope: self;
			yourself)