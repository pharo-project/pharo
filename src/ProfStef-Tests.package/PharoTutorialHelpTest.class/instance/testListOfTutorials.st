tests
testListOfTutorials
	| helpTopic |
	helpTopic  := PharoTutorialHelp listOfTutorials.
	self assert: (helpTopic notNil).
	self assert: (helpTopic title =  'List of tutorials'  )