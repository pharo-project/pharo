Class {
	#name : #MetacelloVersionSpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsCore-Specs'
}

{ #category : #tests }
MetacelloVersionSpecTestCase >> testVersionMergeSpec [
	| versionA versionB version |
	versionA := self versionSpec
		blessing: #baseline;
		versionString: '1.0';
		description: 'A description';
		author: 'dkh';
		timestamp: '1/24/2012 09:59';
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		yourself.
	versionB := self versionSpec
		blessing: #release;
		versionString: '1.1';
		description: 'A FULL description';
		author: 'DaleHenrichs';
		timestamp: '1/24/2012 10:22';
		preLoadDoIt: #preLoadDoItB;
		postLoadDoIt: #postLoadDoItB;
		yourself.
	version := versionA mergeSpec: versionB.
	self assert: version blessing value equals: #release.
	self assert: version versionString value equals: '1.1'.
	self assert: version description value equals: 'A FULL description'.
	self assert: version author value equals: 'DaleHenrichs'.
	self assert: version timestamp value equals: '1/24/2012 10:22'.
	self assert: version preLoadDoIt value identicalTo: #preLoadDoItB.
	self assert: version postLoadDoIt value identicalTo: #postLoadDoItB
]

{ #category : #tests }
MetacelloVersionSpecTestCase >> testVersionSpec [
	| version |
	version := self versionSpec
		blessing: #baseline;
		versionString: '1.0';
		description: 'A description';
		author: 'dkh';
		timestamp: '1/24/2012 09:59';
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		yourself.
	self assert: version blessing value equals: #baseline.
	self assert: version versionString value equals: '1.0'.
	self assert: version description value equals: 'A description'.
	self assert: version author value equals: 'dkh'.
	self assert: version timestamp value equals: '1/24/2012 09:59'.
	self assert: version preLoadDoIt value identicalTo: #preLoadDoIt.
	self assert: version postLoadDoIt value identicalTo: #postLoadDoIt.
	self should: [ version preLoadDoIt: '' ] raise: Error.
	self should: [ version postLoadDoIt: '' ] raise: Error
]

{ #category : #tests }
MetacelloVersionSpecTestCase >> testVersionSpecCreateVersion [
	| spec version |
	spec := self versionSpec
		blessing: #baseline;
		versionString: '1.0';
		description: 'A description';
		author: 'dkh';
		timestamp: '1/24/2012 09:59';
		preLoadDoIt: #preLoadDoIt;
		postLoadDoIt: #postLoadDoIt;
		yourself.
	version := spec createVersion.
	self assert: version class identicalTo: spec versionClass.
	self assert: version spec identicalTo: spec.
	self assert: version versionNumber asString equals: '1.0'.
	self assert: version blessing equals: #baseline.
	self assert: version versionString equals: '1.0'.
	self assert: version description equals: 'A description'.
	self assert: version author equals: 'dkh'.
	self assert: version timestamp equals: '1/24/2012 09:59'
]
