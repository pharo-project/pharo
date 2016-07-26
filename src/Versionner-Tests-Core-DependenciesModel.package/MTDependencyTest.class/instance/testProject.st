as yet unclassified
testProject
	| dependency |
	
	dependency := project dependencyNamed: 'Core'.
	self assert: dependency project equals: project.
	
	dependency := (project dependencyNamed: 'XML-Writer-Core').
	self assert: dependency project equals: project.

	dependency := (project dependencyNamed: 'Collections-Support').
	self assert: dependency project equals: project.
