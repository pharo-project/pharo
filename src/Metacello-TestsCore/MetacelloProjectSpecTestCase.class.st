Class {
	#name : #MetacelloProjectSpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsCore-Specs'
}

{ #category : #tests }
MetacelloProjectSpecTestCase >> testProjectMergeSpec [
	| projectA projectB project |
	projectA := self projectSpec
		name: 'Project';
		className: 'ConfigurationOfProject';
		versionString: '1.0';
		loads: #('MyPackage');
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		yourself.
	projectB := self projectSpec
		name: 'Project';
		className: 'ConfigurationOfProjectB';
		versionString: #stable;
		operator: #<;
		loads: #('MyPackage' 'MyTests');
		preLoadDoIt: #preLoadDoItB;
		postLoadDoIt: #postLoadDoItB;
		yourself.
	project := projectA mergeSpec: projectB.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProjectB'.
	self assert: project versionString equals: #stable.
	self assert: project operator identicalTo: #<.
	self assert: project loads equals: #('MyPackage' 'MyTests').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoItB.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoItB
]

{ #category : #tests }
MetacelloProjectSpecTestCase >> testProjectSpec [
	| project |
	project := self projectSpec.
	project
		name: 'Project';
		className: 'ConfigurationOfProject';
		versionString: '1.0';
		versionString: #stable;
		operator: #<;
		operator: nil;
		loads: 'MyPackage';
		loads: #('MyPackage' 'MyTests');
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		yourself.
	self assert: project name equals: 'Project'.
	self assert: project className equals: 'ConfigurationOfProject'.
	self assert: project versionString equals: #stable.
	self assert: project operator identicalTo: #>=.
	self assert: project loads equals: #('MyPackage' 'MyTests').
	self assert: project preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: project postLoadDoIt value identicalTo: #postLoadDoIt.
	self should: [ project includes: #() ] raise: Error.
	self should: [ project requires: #() ] raise: Error.
	self should: [ project answers: #() ] raise: Error.
	project projectDo: [ :prjct | self assert: project identicalTo: prjct ] packageDo: [ :ignored | self assert: false ] groupDo: [ :ignored | self assert: false ].
	self should: [ project preLoadDoIt: '' ] raise: Error.
	self should: [ project postLoadDoIt: '' ] raise: Error
]
