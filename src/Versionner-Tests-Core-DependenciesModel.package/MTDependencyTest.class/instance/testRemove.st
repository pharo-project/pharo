as yet unclassified
testRemove
	(project dependencyNamed: 'XML-Writer-Core') remove.
	self assert: ((project dependencyNamed:  'XML-Writer-Core') = nil).