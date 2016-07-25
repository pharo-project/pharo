*GT-InspectorExtensions-Core
allProjectsDo: aBlock withTraversedProjects: aCollection
	self projects do: [ :each |
		(each version notNil and: [(aCollection includes: each version) not]) 
		ifTrue: [
			aCollection add: each version.
			aBlock value: each version.
			each version allProjectsDo: aBlock withTraversedProjects: aCollection ] ]