tests
testFirstIconSecondIcon
		testedInstance firstIcon: #thumbsUp secondIcon: #thumbsDown.
	self assert: testedInstance firstButton icon == #thumbsUp asIcon.
	self assert: testedInstance secondButton icon == #thumbsDown asIcon