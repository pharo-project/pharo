Class {
	#name : 'MetacelloVersionSpecTest',
	#superclass : 'MetacelloAbstractSpecTest',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloVersionSpecTest >> testVersionMergeSpec [

	| versionA versionB version |
	versionA := self versionSpec
		            preLoadDoIt: #preLoadDoIt;
		            postLoadDoIt: #postLoadDoIt;
		            yourself.
	versionB := self versionSpec
		            preLoadDoIt: #preLoadDoItB;
		            postLoadDoIt: #postLoadDoItB;
		            yourself.
	version := versionA mergeSpec: versionB.
	self assert: version preLoadDoIt value identicalTo: #preLoadDoItB.
	self assert: version postLoadDoIt value identicalTo: #postLoadDoItB
]

{ #category : 'tests' }
MetacelloVersionSpecTest >> testVersionMergeSpec2 [

	| versionA versionB version repository package group projectReferenceSpec |
	versionA := self versionSpec
		            repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		            repository: '/opt/gemstone/repository';
		            yourself.
	versionA packagesSpec
		merge: (self packageSpec
				 name: 'Package';
				 requires: 'AnotherPackage';
				 yourself);
		merge: (self groupSpec
				 name: 'Platform';
				 includes: 'Core';
				 yourself);
		merge: (MetacelloProjectReferenceSpec projectReference: (self projectSpec
					  name: 'Project';
					  className: 'BaselineOfProjectA';
					  repository: 'http://example.com/repository' username: 'dkh' password: 'password';
					  repository: '/opt/gemstone/repository';
					  yourself)).
	versionB := self versionSpec
		            repository: 'http://example.com/repository' username: 'DaleHenrichs' password: 'secret';
		            repository: '/opt/gemstone/repo';
		            yourself.
	versionB packagesSpec
		merge: (self packageSpec
				 name: 'Package';
				 requires: 'AndAnotherPackage';
				 yourself);
		merge: (self groupSpec
				 name: 'Platform';
				 includes: 'Tests';
				 yourself);
		merge: (MetacelloProjectReferenceSpec projectReference: (self projectSpec
					  name: 'Project';
					  className: 'BaselineOfProjectA';
					  repository: 'http://example.com/repository' username: 'DaleHenrichs' password: 'secret';
					  repository: '/opt/gemstone/repo';
					  yourself)).
	version := versionA mergeSpec: versionB.
	repository := version repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := version repositories map at: '/opt/gemstone/repo' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := version repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'.
	package := version packagesSpec packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	projectReferenceSpec := version packagesSpec packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	projectReferenceSpec repositories map at: '/opt/gemstone/repository' ifPresent: [ self assert: false ].
	repository := projectReferenceSpec repositories map at: '/opt/gemstone/repo' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := projectReferenceSpec repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'.
	group := version packagesSpec packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	self assert: (group includes includes: 'Core').
	self assert: (group includes includes: 'Tests')
]

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
		add: (self projectSpec
				 name: 'Project';
				 className: 'BaselineOfProjectA';
				 loads: #( 'MyPackage' 'MyTests' );
				 preLoadDoIt: #preLoadDoItB;
				 postLoadDoIt: #postLoadDoItB;
				 yourself);
		yourself.
	repository := version repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := version repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'.
	package := version packagesSpec packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	group := version packagesSpec packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	self assert: (group includes includes: 'Core').
	projectReferenceSpec := version packagesSpec packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	version projectDo: [ :prjct | prjct == projectReferenceSpec ] packageDo: [ :pkg | pkg == package ] groupDo: [ :grp | grp == group ]
]

{ #category : 'specs' }
MetacelloVersionSpecTest >> versionSpec [

	^ MetacelloVersionSpec project: self project
]
