"
A PBVersionInfoTest is a test class for testing the behavior of PBVersionInfo
"
Class {
	#name : #PBVersionInfoTest,
	#superclass : #TestCase,
	#category : 'Pharo30Bootstrap-Tests'
}

{ #category : #'test-default-values' }
PBVersionInfoTest >> testDefaultBuildIsUnknown [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo buildNumber equals: -1
]

{ #category : #'test-default-values' }
PBVersionInfoTest >> testDefaultCommitHashIsUnknown [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo commitHash equals: 'UNKNOWN_COMMIT'.
]

{ #category : #'test-default-values' }
PBVersionInfoTest >> testDefaultMinorVersionIsZero [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo minor equals: 0.
]

{ #category : #'test-default-values' }
PBVersionInfoTest >> testDefaultPatchVersionIsZero [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo patch equals: 0.
]

{ #category : #'test-default-values' }
PBVersionInfoTest >> testDefaultSuffixIsEmpty [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo suffix equals: nil.
]

{ #category : #'test-semantic-version' }
PBVersionInfoTest >> testFromIncompleteSemanticVersionStartingWithV [

	| versionInfo |
	versionInfo := PBVersionInfo fromSemanticVersion: 'v70-alpha'.
	
	self assert: versionInfo major equals: 70.
	self assert: versionInfo minor equals: 0.
	self assert: versionInfo patch equals: 0.
	self assert: versionInfo suffix equals: 'alpha'.
]

{ #category : #'test-git-long-describe' }
PBVersionInfoTest >> testFromLongDescribeWithNumberOfCommitsExtractsCommitHash [

	| versionInfo |
	versionInfo := PBVersionInfo fromLongGitDescribe: 'v1.5.3-21-g975b'.	
	self assert: versionInfo commitHash equals: '975b'.
]

{ #category : #'test-git-long-describe' }
PBVersionInfoTest >> testFromLongDescribeWithNumberOfCommitsExtractsSemanticVersion [

	| versionInfo |
	versionInfo := PBVersionInfo fromLongGitDescribe: 'v1.5.3-21-g975b'.
	
	self assert: versionInfo major equals: 1.
	self assert: versionInfo minor equals: 5.
	self assert: versionInfo patch equals: 3.
	self assert: versionInfo suffix equals: nil.
	self assert: versionInfo commitHash equals: '975b'.
]

{ #category : #'test-git-long-describe' }
PBVersionInfoTest >> testFromLongDescribeWithSuffix [

	| versionInfo |
	versionInfo := PBVersionInfo fromLongGitDescribe: 'v1.5.3-suffix-21-g975b'.
	
	self assert: versionInfo major equals: 1.
	self assert: versionInfo minor equals: 5.
	self assert: versionInfo patch equals: 3.
	self assert: versionInfo suffix equals: 'suffix'.
	self assert: versionInfo commitHash equals: '975b'.
]

{ #category : #'test-semantic-version' }
PBVersionInfoTest >> testFromSemanticVersionStartingWithV [

	| versionInfo |
	versionInfo := PBVersionInfo fromSemanticVersion: 'v1.2.3-alpha'.
	
	self assert: versionInfo major equals: 1.
	self assert: versionInfo minor equals: 2.
	self assert: versionInfo patch equals: 3.
	self assert: versionInfo suffix equals: 'alpha'.
]

{ #category : #'test-semantic-version' }
PBVersionInfoTest >> testFromSemanticVersionStartingWithoutV [

	| versionInfo |
	versionInfo := PBVersionInfo fromSemanticVersion: '1.2.3-alpha'.
	
	self assert: versionInfo major equals: 1.
	self assert: versionInfo minor equals: 2.
	self assert: versionInfo patch equals: 3.
	self assert: versionInfo suffix equals: 'alpha'.
]

{ #category : #'test-semantic-version' }
PBVersionInfoTest >> testFromSemanticVersionWithInvalidMajorFails [

	self should: [ PBVersionInfo fromSemanticVersion: 'Invalid-alpha' ] raise: Error.
]

{ #category : #'test-semantic-version' }
PBVersionInfoTest >> testFromSemanticVersionWithoutSuffix [

	| versionInfo |
	versionInfo := PBVersionInfo fromSemanticVersion: 'v1.2.3'.
	
	self assert: versionInfo major equals: 1.
	self assert: versionInfo minor equals: 2.
	self assert: versionInfo patch equals: 3.
	self assert: versionInfo suffix equals: nil.
]

{ #category : #tests }
PBVersionInfoTest >> testGivenBuildIsGiven [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo buildNumber: 200.
	self assert: versionInfo buildNumber equals: 200.
]

{ #category : #tests }
PBVersionInfoTest >> testGivenCommitHashIsGiven [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo commitHash: 'abcabcabcabcabcabc'.
	self assert: versionInfo commitHash equals: 'abcabcabcabcabcabc'.
]

{ #category : #tests }
PBVersionInfoTest >> testGivenMinorVersionIsGiven [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo minor: 17.
	self assert: versionInfo minor equals: 17.
]

{ #category : #tests }
PBVersionInfoTest >> testGivenPatchVersionIsGiven [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo patch: 88.
	self assert: versionInfo patch equals: 88.
]

{ #category : #tests }
PBVersionInfoTest >> testGivenSuffixIsGiven [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo suffix: 'SNAPSHOT'.
	self assert: versionInfo suffix equals: 'SNAPSHOT'.
]

{ #category : #tests }
PBVersionInfoTest >> testNilMajorVersionFails [

	self should: [ PBVersionInfo major: nil ] raise: Error
]

{ #category : #'test-default-values' }
PBVersionInfoTest >> testNoDefaultMajorVersionFails [

	self should: [ PBVersionInfo new ] raise: Error
]

{ #category : #tests }
PBVersionInfoTest >> testProvidedMajorVersionIsStored [

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo major equals: 7.
]
