updating
themeChanged
	
	rowColors at: 1 put: (	
		rowColors asSet size = 1 
			ifTrue: [  self theme backgroundColor ]
			ifFalse: [ rowColors at: 1 put: self theme backgroundColor contrastingColorAdjustment ]
		).
		
	rowColors at: 2 put: self theme backgroundColor .
	
	super themeChanged.