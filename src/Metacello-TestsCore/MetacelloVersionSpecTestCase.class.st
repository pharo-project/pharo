Class {
	#name : 'MetacelloVersionSpecTestCase',
	#superclass : 'MetacelloSpecTestCase',
	#category : 'Metacello-TestsCore-Specs',
	#package : 'Metacello-TestsCore',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloVersionSpecTestCase >> testVersionMergeSpec [

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
MetacelloVersionSpecTestCase >> testVersionMergeSpec2 [
	| versionA versionB version repository package group projectReferenceSpec |
	versionA := self versionSpec
		repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		repository: '/opt/gemstone/repository';
		yourself.
	versionA packages
		merge:
			(self packageSpec
				name: 'Package';
				requires: 'AnotherPackage';
				yourself);
		merge:
			(self groupSpec
				name: 'Platform';
				includes: 'Core';
				yourself);
		merge:
			(self projectReferenceSpec
				name: 'Project';
				projectReference:
					(self projectSpec
						name: 'Project';
						projectPackage: self project packageSpec;
						className: 'ConfigurationOfProjectA';
						repository: 'http://example.com/repository' username: 'dkh' password: 'password';
						repository: '/opt/gemstone/repository';
						yourself));
		yourself.
	versionB := self versionSpec
		repository: 'http://example.com/repository' username: 'DaleHenrichs' password: 'secret';
		repository: '/opt/gemstone/repo';
		yourself.
	versionB packages
		merge:
			(self packageSpec
				name: 'Package';
				requires: 'AndAnotherPackage';
				yourself);
		merge:
			(self groupSpec
				name: 'Platform';
				includes: 'Tests';
				yourself);
		merge:
			(self projectReferenceSpec
				name: 'Project';
				projectReference:
					(self projectSpec
						name: 'Project';
						projectPackage: self project packageSpec;
						className: 'ConfigurationOfProjectA';
						repository: 'http://example.com/repository' username: 'DaleHenrichs' password: 'secret';
						repository: '/opt/gemstone/repo';
						yourself));
		yourself.
	version := versionA mergeSpec: versionB.
	repository := version repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := version repositories map at: '/opt/gemstone/repo' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := version repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'.
	package := version packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	projectReferenceSpec := version packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	repository := projectReferenceSpec repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := projectReferenceSpec repositories map at: '/opt/gemstone/repo' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := projectReferenceSpec repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'.
	group := version packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	self assert: (group includes includes: 'Core').
	self assert: (group includes includes: 'Tests')
]

{ #category : 'tests' }
MetacelloVersionSpecTestCase >> testVersionSpec [

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
MetacelloVersionSpecTestCase >> testVersionSpec2 [
	| version projectReferenceSpec group package repository |
	version := self versionSpec
		repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		repository: '/opt/gemstone/repository';
		yourself.
	version packages
		add:
			(self packageSpec
				name: 'Package';
				requires: 'AnotherPackage';
				includes: 'IncludedPackage';
				file: 'Package-dkh.1';
				preLoadDoIt: #preLoadDoIt;
				postLoadDoIt: #postLoadDoIt;
				yourself);
		add:
			(self packageSpec
				name: 'AnotherPackage';
				yourself);
		add:
			(self packageSpec
				name: 'IncludedPackage';
				yourself);
		add:
			(self groupSpec
				name: 'Platform';
				includes: 'Core';
				yourself);
		add:
			(self projectSpec
				name: 'Project';
				className: 'ConfigurationOfProjectA';
				loads: #('MyPackage' 'MyTests');
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
	package := version packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
	self assert: package name equals: 'Package'.
	group := version packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
	self assert: (group includes includes: 'Core').
	projectReferenceSpec := version packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
	self assert: projectReferenceSpec projectName equals: 'Project'.
	version projectDo: [ :prjct | prjct == projectReferenceSpec ] packageDo: [ :pkg | pkg == package ] groupDo: [ :grp | grp == group ]
]

{ #category : 'tests' }
MetacelloVersionSpecTestCase >> testVersionSpecCreateVersion [

	| spec version |
	spec := self versionSpec
		        preLoadDoIt: #preLoadDoIt;
		        postLoadDoIt: #postLoadDoIt;
		        yourself.
	version := spec createVersion.
	self assert: version class identicalTo: spec versionClass.
	self assert: version spec identicalTo: spec
]
