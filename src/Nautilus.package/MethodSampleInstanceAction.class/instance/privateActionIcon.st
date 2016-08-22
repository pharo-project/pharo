order
privateActionIcon

	 ^IconicButton new 
			target: self browser;
			actionSelector: #runExamplarMethod:;
		 	arguments: { method };
			labelGraphic: (self iconNamed: #smallDoItIcon) ;
			color: Color transparent;
			extent: self iconExtent;
			helpText: 'Get the class sample (predefined instance) in a inspector to tweak it and have fun with it';
			borderWidth: 0 