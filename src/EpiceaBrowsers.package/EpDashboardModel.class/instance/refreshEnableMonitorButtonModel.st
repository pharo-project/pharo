refreshing
refreshEnableMonitorButtonModel
		
	| theState theIconSelector |
	self monitor isEnabled 
		ifTrue: [ 
			theState := 'enabled'.
			theIconSelector := #testGreen ] 
		ifFalse: [ 
			theState := 'disabled'.
			theIconSelector := #testNotRun ].
	
	enableMonitorButtonModel
		state: self monitor isEnabled;
		label: 'Monitor ', theState;
		icon: (self iconNamed: theIconSelector).
