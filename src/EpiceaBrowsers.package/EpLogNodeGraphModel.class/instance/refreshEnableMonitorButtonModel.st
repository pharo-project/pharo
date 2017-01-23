refreshing
refreshEnableMonitorButtonModel
		
	| theState theIconSelector |
	self monitor isEnabled 
		ifTrue: [ 
			theState := 'on'.
			theIconSelector := #testGreen ] 
		ifFalse: [ 
			theState := 'off'.
			theIconSelector := #testNotRun ].
	
	enableMonitorButtonModel
		state: self monitor isEnabled;
		label: 'Monitor ', theState;
		icon: (self iconNamed: theIconSelector).
