tests
testNavigation
	self assert: player next title equals: 'second'.
	self assert: player currentLesson title equals: 'second'.
	
	self assert: player next title equals: 'third'.
	self assert: player currentLesson title equals: 'third'.
	
	self assert: player next title equals: 'third'.
	self assert: player currentLesson title equals: 'third'.
	
	self assert: player previous title equals: 'second'.
	self assert: player currentLesson title equals: 'second'.
	
	self assert: player previous title equals: 'first'.
	self assert: player currentLesson title equals: 'first'.
	
	self assert: player previous title equals: 'first'.
	self assert: player currentLesson title equals: 'first'.