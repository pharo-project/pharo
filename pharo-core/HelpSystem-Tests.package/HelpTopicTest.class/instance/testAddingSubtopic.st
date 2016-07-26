testing
testAddingSubtopic

	|subtopic returned|
	subtopic := self defaultTestClass named: 'Subtopic'.
	returned := topic addSubtopic: subtopic.
	self assert: returned == subtopic.
	self assert: (topic subtopics includes: subtopic) 