helper
ignoreIfFail: aBlock
	^ [ aBlock value ] ifError: [  ]