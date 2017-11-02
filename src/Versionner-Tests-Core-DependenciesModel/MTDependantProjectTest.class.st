"
Test clas for a dependant project
"
Class {
	#name : #MTDependantProjectTest,
	#superclass : #TestCase,
	#category : 'Versionner-Tests-Core-DependenciesModel'
}

{ #category : #private }
MTDependantProjectTest >> referenceDependency [
	| version |
	
	version := MTVersion new
					name: '1.0';
					yourself.
					
	^ (MTDependantProject newNamed: 'LibXYZ')
			configurationClass: self class;
			repositories: #('http://smalltalkhub.com/mc/dummy/XYZ/main', 'http://github.com/XYZ');
			displayName: 'XYZ';
			parent: nil;
			version: version;
			yourself.
]

{ #category : #tests }
MTDependantProjectTest >> testEquals [
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
]
