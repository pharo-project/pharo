Class {
	#name : 'MetacelloVersionSpecTest',
	#superclass : 'MetacelloAbstractSpecTest',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloVersionSpecTest >> testVersionSpec [

	| version |
	version := self versionSpec
		           preLoadDoIt: #preLoadDoIt;
		           postLoadDoIt: #postLoadDoIt;
		           yourself.
	self assert: version preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: version postLoadDoIt value identicalTo: #postLoadDoIt.
	self should: [ version preLoadDoIt: '' ] raise: Error.
	self should: [ version postLoadDoIt: '' ] raise: Error
]

{ #category : 'tests' }
MetacelloVersionSpecTest >> testVersionSpec2 [

	| version projectReferenceSpec group package repository |
	version := self versionSpec
		           repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		           repository: '/opt/gemstone/repository';
		           yourself.
	version packagesSpec
		add: (self packageSpec
				 name: 'Package';
				 requires: 'AnotherPackage';
				 includes: 'IncludedPackage';
				 preLoadDoIt: #preLoadDoIt;
				 postLoadDoIt: #postLoadDoIt;
				 yourself);
		add: (self packageSpec
				 name: 'AnotherPackage';
				 yourself);
		add: (self packageSpec
				 name: 'IncludedPackage';
				 yourself);
		add: (self groupSpec
				 name: 'Platform';
				 includes: 'Core';
				 yourself);
		add: (MetacelloProjectReferenceSpec projectReference: (self projectSpec
					  name: 'Project';
					  className: 'BaselineOfProjectA';
					  loads: #( 'MyPackage' 'MyTests' );
					  preLoadDoIt: #preLoadDoItB;
					  postLoadDoIt: #postLoadDoItB;
					  yourself)).
	repository := version repositories repositoryNamed: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := version repositories repositoryNamed: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'.
	package := version packagesSpec loadableSpecNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	group := version packagesSpec loadableSpecNamed: 'Platform' ifAbsent: [ self assert: false ].
	self assert: (group includes includes: 'Core').
	projectReferenceSpec := version packagesSpec loadableSpecNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'
]

{ #category : 'specs' }
MetacelloVersionSpecTest >> versionSpec [

	^ MetacelloVersionSpec project: self project
]
