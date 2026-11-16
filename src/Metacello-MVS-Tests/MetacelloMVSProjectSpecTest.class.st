"
I test what MVS reads from a project spec: its repository, version and path, through Metacello's repository objects (the graph), and what its version means (the resolver): a minimum, a floating tag, or no version.
"
Class {
	#name : 'MetacelloMVSProjectSpecTest',
	#superclass : 'MetacelloTestCase',
	#instVars : [
		'graph',
		'resolver'
	],
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'running' }
MetacelloMVSProjectSpecTest >> setUp [

	super setUp.
	graph := MetacelloMVSGraph on: MetacelloMVSRecordedSource new.
	resolver := MetacelloMVSResolver on: graph
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> spec: aRepositoryString [

	^ MetacelloMVSResolverTest specAt: aRepositoryString
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> testABranchOrNoVersionIsNotAMinimum [

	self
		deny: (resolver isVersioned: (self spec: 'github://o/n:master/src'));
		deny: (resolver isVersioned: (self spec: 'github://o/n'));
		assert: (resolver isVersioned: (self spec: 'github://o/n:v1.6.0/src'))
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> testADirectoryIsNamedByItsWholeDescription [

	self assert: (graph repositoryUrlOf: (self spec: 'tonel:///tmp/n/src')) equals: 'tonel:///tmp/n/src'
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> testAFloatingVersionIsTheFirstOfItsSeries [

	self
		assert: (resolver isFloating: (self spec: 'github://o/n:v1.6.x/src'));
		assert: (resolver minimumOf: (self spec: 'github://o/n:v1.6.x/src'))
		equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.6.0');
		assert: (resolver minimumOf: (self spec: 'github://o/n:v1.x.x/src'))
		equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.0.0');
		deny: (resolver isFloating: (self spec: 'github://o/n:v1.6.0/src'))
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> testAVersionIsItsMinimum [

	self
		assert: (resolver minimumOf: (self spec: 'github://o/n:v1.2.3/src'))
		equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3')
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> testOnlyRepositoriesThatPublishVersionsHaveVersions [

	| source |
	source := MetacelloMVSRecordedSource new.
	source
		repository: 'github://o/n' versions: #( 'v1.6.0' );
		repository: 'github://o/m' versions: #(  );
		repository: 'tonel:///tmp/n' versions: #(  ).
	graph := MetacelloMVSGraph on: source.
	self
		assert: (graph hasVersions: (self spec: 'github://o/n:v1.6.0/src') ifFail: [ :why | self fail: why ]);
		deny: (graph hasVersions: (self spec: 'github://o/m:v1.6.0/src') ifFail: [ :why | self fail: why ]);
		deny: (graph hasVersions: (self spec: 'tonel:///tmp/n') ifFail: [ :why | self fail: why ])
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> testSplitsARepositoryString [

	| spec |
	spec := self spec: 'github://pharo-graphics/Alexandrie:master/src'.
	self
		assert: (graph repositoryUrlOf: spec) equals: 'github://pharo-graphics/Alexandrie';
		assert: (graph versionStringOf: spec) equals: 'master'
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> testWithRefWithoutPath [

	self assert: (graph versionStringOf: (self spec: 'github://owner/name:v1.2.0')) equals: 'v1.2.0'
]
