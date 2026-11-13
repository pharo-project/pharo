"
I test MetacelloSemanticVersionNumber read from git tags: strict and tolerated forms, floating tags, prereleases and their order.
"
Class {
	#name : 'MetacelloSemanticVersionNumberTest',
	#superclass : 'MetacelloTestCase',
	#category : 'Metacello-Tests',
	#package : 'Metacello-Tests'
}

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testAPrereleaseComesBeforeItsRelease [

	self
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v2.0.0-rc1') < (MetacelloSemanticVersionNumber fromVersionString: 'v2.0.0');
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v2.0.0-rc1') > (MetacelloSemanticVersionNumber fromVersionString: 'v1.9.0');
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v2.0.0-rc1') < (MetacelloSemanticVersionNumber fromVersionString: 'v2.0.0-rc2')
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testATwoNumberVersionIsItsFirstPatch [

	| version |
	version := MetacelloSemanticVersionNumber fromVersionString: 'v1.2'.
	self
		assert: version equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.0');
		assert: version < (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.1');
		assert: version string equals: 'v1.2'
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testCanonicalVersions [

	self assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3') isCanonical.
	self assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3-rc1') isCanonical.
	#( 'v1.02' 'v1.2' '1.2.7' 'v01.2.3' ) do: [ :each | self deny: (MetacelloSemanticVersionNumber fromVersionString: each) isCanonical ]
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testEquality [

	self
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3') equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3');
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3') hash equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3') hash;
		deny: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3') = (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3-rc1')
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testFloatingVersions [

	self
		assert: (MetacelloSemanticVersionNumber isSeries: 'v1.6.x');
		assert: (MetacelloSemanticVersionNumber isSeries: 'v1.x.x');
		assert: (MetacelloSemanticVersionNumber fromSeries: 'v1.6.x') equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.6.0');
		assert: (MetacelloSemanticVersionNumber fromSeries: 'v1.x.x') equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.0.0')
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testNonCanonicalSpellingsAreVersions [

	self
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.02') equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.0');
		assert: (MetacelloSemanticVersionNumber fromVersionString: '1.2.7') equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.7');
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v01.2.3') equals: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3');
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.02') string equals: 'v1.02'
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testOnlyStrictSpellingsAreSemanticVersions [
	"Only what is no version at all; non-canonical versions are accepted, see testNonCanonicalTagsAreVersions"

	self assert: (MetacelloSemanticVersionNumber isSemanticVersion: 'v10.20.30').
	#( 'v3' 'v1.6.x' 'release-3' 'v1.2.3.4' 'master' 'v1.2.3-' 'v.2.0' 'vv1.2' ) do: [ :each | self deny: (MetacelloSemanticVersionNumber isSemanticVersion: each) ]
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testOrdersNumerically [

	self
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.10') > (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.9');
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.10.0') > (MetacelloSemanticVersionNumber fromVersionString: 'v1.9.9');
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v2.0.0') > (MetacelloSemanticVersionNumber fromVersionString: 'v1.99.99')
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testOtherShapesAreNotFloatingVersions [

	#( 'vx.x.x' 'v1.x.2' 'v1.x' 'v1.2.x.x' 'v1.6.0' '1.2.x' ) do: [ :each | self deny: (MetacelloSemanticVersionNumber isSeries: each) ]
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testPrintsAsWritten [

	self
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3') string equals: 'v1.2.3';
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3-rc1') string equals: 'v1.2.3-rc1';
		assert: (MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3') printString equals: 'v1.2.3'
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testReadsAPrerelease [

	| version |
	version := MetacelloSemanticVersionNumber fromVersionString: 'v2.0.0-rc1'.
	self
		assert: version prerelease equals: 'rc1';
		assert: version isPrerelease
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testReadsAVersionString [

	| version |
	version := MetacelloSemanticVersionNumber fromVersionString: 'v1.2.3'.
	self
		assert: version major equals: 1;
		assert: version minor equals: 2;
		assert: version patch equals: 3;
		assert: version prerelease isNil
]

{ #category : 'tests' }
MetacelloSemanticVersionNumberTest >> testRejectsAnythingButAVersion [

	self should: [ MetacelloSemanticVersionNumber fromVersionString: 'v1.6.x' ] raise: Error
]
