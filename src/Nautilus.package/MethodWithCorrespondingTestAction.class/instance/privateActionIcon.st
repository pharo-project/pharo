order
privateActionIcon
	"Return the icon for this action"
	| testIcon |
	
	testIcon := self iconNamed: #testNotRunIcon.
	
	testMethod hasPassedTest
		ifTrue: [ testIcon := Smalltalk ui icons iconNamed: #testGreenIcon ].
	testMethod hasFailedTest
		ifTrue: [ testIcon := Smalltalk ui icons iconNamed: #testYellowIcon ].
	testMethod hasErrorTest
		ifTrue: [ testIcon := Smalltalk ui icons iconNamed: #testRedIcon ].
	
	^ IconicButton new
		target: self browser;
		actionSelector: #runTestForAMethod:notifying:;
		arguments:  { method. true};
		labelGraphic: testIcon;
		color: Color transparent;
		extent: 12 @ 12;
		helpText: 'Run the test';
		borderWidth: 0