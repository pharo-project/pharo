refreshing
refresh
		
	| theState theIconSelector |
	activeLogTextModel
		text: self monitor sessionStore writingFileReference basenameWithoutExtension.
	
	logModel
		log: self monitor log;
		refresh.
		
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
		