private - accessing
privateActionIcon

	 ^IconicButton new 
			target: self;
			actionSelector: #runScript;
			labelGraphic: (self iconNamed: #scriptManagerIcon) ;
			color: Color transparent;
			extent: self iconExtent;
			helpText: 'Run the script(s)';
			borderWidth: 0 