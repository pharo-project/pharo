tests
testBuild
	| helpTopic tutorial |
	tutorial := PharoTutorialsHelp.
	helpTopic := PharoTutorialHelpTutorialBuilder buildHelpTopicFrom: tutorial.
	self assert: helpTopic notNil