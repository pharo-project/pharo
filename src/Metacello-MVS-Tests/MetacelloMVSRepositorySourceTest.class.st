"
I test how MetacelloMVSRepositorySource asks the repositories Metacello's platform creates for their versions and baselines, on a test platform and repository.
"
Class {
	#name : 'MetacelloMVSRepositorySourceTest',
	#superclass : 'MetacelloTestCase',
	#instVars : [
		'platform',
		'repository',
		'source'
	],
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'tests' }
MetacelloMVSRepositorySourceTest >> candidateFor: aRepositoryString [
	"The candidate for aRepositoryString at its minimum, for the project Foo"

	| graph spec resolver |
	graph := MetacelloMVSGraph new.
	resolver := MetacelloMVSResolver on: graph.
	spec := graph specFor: aRepositoryString project: 'Foo' loads: #(  ).
	^ resolver candidateFor: spec version: (resolver minimumOf: spec) requiredBy: nil
]

{ #category : 'tests' }
MetacelloMVSRepositorySourceTest >> fooBaseline [

	^ (MetacelloMVSBaselineDefinitions
		   baselineClassNamed: 'BaselineOfFoo'
		   method: 'baseline: spec
	<baseline>
	spec for: #common do: [ spec package: ''Core'' ]'
		   repository: 'github://acme/Foo:v1.0.0') definitions
]

{ #category : 'running' }
MetacelloMVSRepositorySourceTest >> setUp [

	super setUp.
	platform := MetacelloMVSTestPlatform new.
	MetacelloPlatform current: platform.
	repository := MetacelloMVSTestRepository new.
	platform repository: 'github://acme/Foo' is: repository.
	source := MetacelloMVSRepositorySource new
]

{ #category : 'tests' }
MetacelloMVSRepositorySourceTest >> testARepositoryWithoutVersionsAnswersNone [

	self assert: (source versionsOf: 'github://acme/Bar' ifFail: [ :text | text ]) isEmpty
]

{ #category : 'tests' }
MetacelloMVSRepositorySourceTest >> testAVersionWithoutBaselineIsAnError [

	repository version: 'v1.0.0' baseline: nil.
	self assert: ((source baselineOf: (self candidateFor: 'github://acme/Foo:v1.0.0')) problems anySatisfy: [ :each |
			 each first = #missingBaseline ])
]

{ #category : 'tests' }
MetacelloMVSRepositorySourceTest >> testAWrittenPathIsAWarning [

	repository version: 'v1.0.0' baseline: self fooBaseline.
	self assert: ((source baselineOf: (self candidateFor: 'github://acme/Foo:v1.0.0/src')) problems anySatisfy: [ :each |
			 each first = #writtenPath ])
]

{ #category : 'tests' }
MetacelloMVSRepositorySourceTest >> testAnswersTheVersionsOfTheRepository [

	repository version: 'v1.0.0' baseline: self fooBaseline.
	self
		assert: ((source versionsOf: 'github://acme/Foo' ifFail: [ :text | self fail: text ]) collect: [ :each | each name ]) asArray
		equals: #( 'v1.0.0' )
]

{ #category : 'tests' }
MetacelloMVSRepositorySourceTest >> testReadsTheBaselineFromTheRepository [

	| baseline |
	repository version: 'v1.0.0' baseline: self fooBaseline.
	baseline := source baselineOf: (self candidateFor: 'github://acme/Foo:v1.0.0').
	self
		assert: baseline hasBaseline;
		assert: baseline repositoryDescription equals: 'github://acme/Foo:v1.0.0';
		assert: baseline problems isEmpty
]
