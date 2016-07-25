order
privateActionIcon
	"Return the icon for this action"
	| testIcon |
	
	testIcon := self iconNamed: #testNotRunIcon.
	
	method hasPassedTest
		ifTrue: [ testIcon := Smalltalk ui icons iconNamed: #testGreenIcon ].
	method hasFailedTest
		ifTrue: [ testIcon := Smalltalk ui icons iconNamed: #testYellowIcon ].
	method hasErrorTest
		ifTrue: [ testIcon := Smalltalk ui icons iconNamed: #testRedIcon ].
	
	^ IconicButton new
		target: self browser;
		actionSelector: #runTestForAMethod:notifying:;
		arguments: {method. true};
		labelGraphic: testIcon;
		color: Color transparent;
		helpText: 'Run the test';
		extent: self iconExtent;
		borderWidth: 0