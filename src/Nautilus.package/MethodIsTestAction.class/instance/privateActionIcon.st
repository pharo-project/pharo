order
privateActionIcon
	"Return the icon for this action"
	
	^ IconicButton new
		target: self browser;
		actionSelector: #runTestForAMethod:notifying:;
		arguments: {method. true};
		labelGraphic: (self iconForTestResultOf: method);
		color: Color transparent;
		helpText: 'Run the test';
		extent: self iconExtent;
		borderWidth: 0