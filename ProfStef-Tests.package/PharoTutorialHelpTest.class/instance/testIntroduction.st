tests
testIntroduction
	| helpTopic |
	helpTopic := PharoTutorialHelp introduction.
	self assert: helpTopic notNil.
	self assert: helpTopic class == HelpTopic.
	self assert: helpTopic title = 'Introduction'