tests
testDefaultsToSmalltalkSyntaxTutorial
	| player |
	player := TutorialPlayer new.
	self assert: (player tutorial isKindOf: PharoSyntaxTutorial).