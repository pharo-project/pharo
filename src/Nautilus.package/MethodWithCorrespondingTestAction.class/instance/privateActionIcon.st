order
privateActionIcon
	"Return the icon for this action"
	
	^ IconicButton new
		target: self browser;
		actionSelector: #runTestForAMethod:notifying:;
		arguments:  { method. true};
		labelGraphic: (self iconForTestResultOf: testMethod) ;
		color: Color transparent;
		extent: 12 @ 12;
		helpText: 'Run the test';
		borderWidth: 0