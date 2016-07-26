decompiling
testJumpAheadTarget: label

	jumpAheadStacks at: label ifPresent: [:stack |
		[stack isEmpty] whileFalse: [self jumpAheadTarget: label]
	]