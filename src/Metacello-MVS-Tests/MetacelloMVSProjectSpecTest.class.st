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

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> setUp [

	super setUp.
	graph := MetacelloMVSGraph new.
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
MetacelloMVSProjectSpecTest >> testOnlyRepositoriesWithAVersionHaveVersions [

	self
		assert: (graph hasVersions: (self spec: 'github://o/n:v1.6.0/src'));
		deny: (graph hasVersions: (self spec: 'filetree:///tmp/n'))
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> testSplitsARepositoryString [

	| spec |
	spec := self spec: 'github://pharo-graphics/Alexandrie:master/src'.
	self
		assert: (graph repositoryUrlOf: spec) equals: 'github://pharo-graphics/Alexandrie';
		assert: (graph versionStringOf: spec) equals: 'master';
		assert: (graph pathOf: spec) equals: 'src'
]

{ #category : 'tests' }
MetacelloMVSProjectSpecTest >> testWithRefWithoutPath [

	| spec |
	spec := self spec: 'github://owner/name:v1.2.0'.
	self
		assert: (graph versionStringOf: spec) equals: 'v1.2.0';
		assert: (graph pathOf: spec) isNil
]
