events
testAnnouncement
	| collection oldCollection suite unitTest |
	unitTest := ClassFactoryForTestCaseTest.
	collection := OrderedCollection new.
	unitTest resetAnnouncer.
	
	self deny: (unitTest shouldAnnounce).
	self deny: (unitTest new shouldAnnounce).
		
	unitTest announcer when: TestCaseAnnouncement do: [:ann | collection add: ann ].

	self assert: (unitTest shouldAnnounce).
	self assert: (unitTest new shouldAnnounce).

	"We run SUnitTest"
	suite := unitTest buildSuite.
	suite run.

	self assert: (collection size = (suite tests size * 2)).
	self assert: (collection select: [:c | c isKindOf: TestCaseStarted]) size = (collection size / 2).
	self assert: (collection select: [:c | c isKindOf: TestCaseEnded]) size =( collection size / 2).
	self assert: (collection allSatisfy: #hasPassed).
	self deny: (collection anySatisfy: #hasFailures).
	
	oldCollection := collection copy.
	unitTest resetAnnouncer.
	unitTest debug: #testClassCreationInDifferentCategories.
	self assert: (collection = oldCollection)