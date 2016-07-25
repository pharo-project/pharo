order
privateActionIcon

	 ^IconicButton new 
			target: self browser;
			actionSelector: #runExampleMethod:;
		 	arguments: { method };
			labelGraphic: (self iconNamed: #smallDoItIcon) ;
			color: Color transparent;
			extent: self iconExtent;
			helpText: 'Run the example';
			borderWidth: 0 