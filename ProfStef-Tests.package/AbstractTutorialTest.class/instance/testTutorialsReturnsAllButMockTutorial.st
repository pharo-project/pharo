tests
testTutorialsReturnsAllButMockTutorial
	| tutorials |
	tutorials := AbstractTutorial tutorials.
	self assert: (tutorials includes: PharoSyntaxTutorial).
	self assert: (tutorials includes: HowToMakeYourOwnTutorial).
	self deny: (tutorials includes: MockTutorial).