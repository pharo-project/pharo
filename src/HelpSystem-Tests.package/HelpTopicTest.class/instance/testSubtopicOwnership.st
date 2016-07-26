testing
testSubtopicOwnership
"Test that when a subtopic is added to a topic then owner of the subtopic is the topic"
	|subtopic owner|
	owner := self defaultTestClass named:  'I am the owner'.
	subtopic := self defaultTestClass named: 'I am the subtopic'.
	
	owner addSubtopic: subtopic.
	
	self assert: (subtopic owner == owner).
