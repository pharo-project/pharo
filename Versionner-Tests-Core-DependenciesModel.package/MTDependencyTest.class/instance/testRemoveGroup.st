as yet unclassified
testRemoveGroup
	(project dependencyNamed: 'Core') remove.
	self assert: (project dependencyNamed: 'Core') = nil.
	self assert: ((project dependencyNamed: 'default') dependencies includes: 'Core') not