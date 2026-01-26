Class {
	#name : 'MetacelloProjectSpecTest',
	#superclass : 'MetacelloAbstractSpecTest',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloProjectSpecTest >> testProjectMergeSpec [

	| projectA projectB project |
	projectA := self projectSpec
		            name: 'Project';
		            className: 'BaselineOfProject';
		            loads: #( 'MyPackage' );
		            preLoadDoIt: #preLoadDoIt;
		            postLoadDoIt: #postLoadDoIt;
		            yourself.
	projectB := self projectSpec
		            name: 'Project';
		            className: 'BaselineOfProjectB';
		            loads: #( 'MyPackage' 'MyTests' );
		            preLoadDoIt: #preLoadDoItB;
		            postLoadDoIt: #postLoadDoItB;
		            yourself.
	project := projectA mergeSpec: projectB.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProjectB'.
	self assert: project loads equals: #( 'MyPackage' 'MyTests' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoItB.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoItB
]

{ #category : 'tests' }
MetacelloProjectSpecTest >> testProjectMergeSpec2 [

	| projectA projectB project repository |
	projectA := self projectSpec
		            name: 'Project';
		            className: 'BaselineOfProject';
		            repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		            repository: '/opt/gemstone/repository';
		            yourself.
	projectB := self projectSpec
		            name: 'Project';
		            className: 'BaselineOfProjectB';
		            repository: 'http://example.com/repository' username: 'DaleHenrichs' password: 'secret';
		            repository: '/opt/gemstone/repo';
		            yourself.
	project := projectA mergeSpec: projectB.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProjectB'.
	self assert: project projectPackage name equals: project className.
	project repositories repositoryNamed: '/opt/gemstone/repository' ifPresent: [ self assert: false ].
	repository := project repositories repositoryNamed: '/opt/gemstone/repo' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := project repositories repositoryNamed: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'
]

{ #category : 'tests' }
MetacelloProjectSpecTest >> testProjectSpec [

	| project |
	project := self projectSpec
		           name: 'Project';
		           className: 'BaselineOfProject';
		           loads: 'MyPackage';
		           loads: #( 'MyPackage' 'MyTests' );
		           preLoadDoIt: #preLoadDoIt;
		           postLoadDoIt: #postLoadDoIt;
		           yourself.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProject'.
	self assert: project loads equals: #( 'MyPackage' 'MyTests' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	self should: [ project includes: #(  ) ] raise: Error.
	self should: [ project requires: #(  ) ] raise: Error.
	self assert: project class equals: MetacelloProjectSpec.
	self should: [ project preLoadDoIt: '' ] raise: Error.
	self should: [ project postLoadDoIt: '' ] raise: Error
]

{ #category : 'tests' }
MetacelloProjectSpecTest >> testProjectSpec2 [

	| project repository |
	project := self projectSpec
		           name: 'Project';
		           className: 'BaselineOfProject';
		           repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		           repository: '/opt/gemstone/repository';
		           yourself.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'BaselineOfProject'.
	self assert: project projectPackage name equals: project className.
	repository := project repositories repositoryNamed: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := project repositories repositoryNamed: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'
]
