Class {
	#name : 'MetacelloProjectSpecTestCase',
	#superclass : 'MetacelloSpecTestCase',
	#category : 'Metacello-TestsCore-Specs',
	#package : 'Metacello-TestsCore',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloProjectSpecTestCase >> testProjectMergeSpec [

	| projectA projectB project |
	projectA := self projectSpec
		            name: 'Project';
		            className: 'ConfigurationOfProject';
		            loads: #( 'MyPackage' );
		            preLoadDoIt: #preLoadDoIt;
		            postLoadDoIt: #postLoadDoIt;
		            yourself.
	projectB := self projectSpec
		            name: 'Project';
		            className: 'ConfigurationOfProjectB';
		            operator: #<;
		            loads: #( 'MyPackage' 'MyTests' );
		            preLoadDoIt: #preLoadDoItB;
		            postLoadDoIt: #postLoadDoItB;
		            yourself.
	project := projectA mergeSpec: projectB.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProjectB'.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #( 'MyPackage' 'MyTests' ).
	self assert: project preLoadDoIt value identicalTo: #preLoadDoItB.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoItB
]

{ #category : 'tests' }
MetacelloProjectSpecTestCase >> testProjectMergeSpec2 [

	| projectA projectB project repository |
	projectA := self projectSpec
		            name: 'Project';
		            className: 'ConfigurationOfProject';
		            repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		            repository: '/opt/gemstone/repository';
		            yourself.
	projectB := self projectSpec
		            name: 'Project';
		            className: 'ConfigurationOfProjectB';
		            repository: 'http://example.com/repository' username: 'DaleHenrichs' password: 'secret';
		            repository: '/opt/gemstone/repo';
		            yourself.
	project := projectA mergeSpec: projectB.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProjectB'.
	self assert: project projectPackage name equals: project className.
	project repositories map at: '/opt/gemstone/repository' ifPresent: [ self assert: false ].
	repository := project repositories map at: '/opt/gemstone/repo' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := project repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'DaleHenrichs'.
	self assert: repository password equals: 'secret'
]

{ #category : 'tests' }
MetacelloProjectSpecTestCase >> testProjectSpec [
	| project |
	project := self projectSpec.
	project
		name: 'Project';
		className: 'ConfigurationOfProject';
		operator: #<;
		operator: nil;
		loads: 'MyPackage';
		loads: #('MyPackage' 'MyTests');
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		yourself.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project operator identicalTo: #>=.
	self assert: project loads equals: #('MyPackage' 'MyTests').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	self should: [ project includes: #() ] raise: Error.
	self should: [ project requires: #() ] raise: Error.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ].
	self should: [ project preLoadDoIt: '' ] raise: Error.
	self should: [ project postLoadDoIt: '' ] raise: Error
]

{ #category : 'tests' }
MetacelloProjectSpecTestCase >> testProjectSpec2 [
	| project repository |
	project := self projectSpec.
	project
		name: 'Project';
		className: 'ConfigurationOfProject';
		repository: 'http://example.com/repository' username: 'dkh' password: 'password';
		repository: '/opt/gemstone/repository';
		yourself.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project projectPackage name equals: project className.
	repository := project repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'directory'.
	repository := project repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
	self assert: repository type equals: 'http'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'
]
