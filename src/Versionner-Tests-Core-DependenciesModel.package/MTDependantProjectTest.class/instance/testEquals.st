tests
testEquals
	| dependency otherDependency |
	
	dependency := self referenceDependency.
	
	otherDependency := self referenceDependency.
	self assert: otherDependency equals: dependency.
					
	otherDependency := self referenceDependency
								name: 'toto';
								yourself.
	self deny: otherDependency = dependency.

	otherDependency := self referenceDependency
								parent: self;
								yourself.
	self deny: otherDependency = dependency.

	otherDependency := self referenceDependency
								configurationClass: Object;
								yourself.
	self deny: otherDependency = dependency.
	
	otherDependency := self referenceDependency
								repositories: #('http://smalltalkhub.com/mc/dummy/XYZ/main', 'http://github.com/LibXYZ');
								yourself.
	self deny: otherDependency = dependency.