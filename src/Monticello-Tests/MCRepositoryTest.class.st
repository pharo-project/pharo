"
Abstract superclass for Repository tests
"
Class {
	#name : #MCRepositoryTest,
	#superclass : #MCTestCase,
	#instVars : [
		'repository'
	],
	#category : #'Monticello-Tests-Repository'
}

{ #category : #testing }
MCRepositoryTest class >> isAbstract [
	^ self = MCRepositoryTest
]

{ #category : #actions }
MCRepositoryTest >> addVersion: aVersion [
	self subclassResponsibility 
]

{ #category : #actions }
MCRepositoryTest >> addVersionWithSnapshot: aSnapshot name: aString [
	| version |
	version := self versionWithSnapshot: aSnapshot name: aString.
	self addVersion: version.
	^ version info
]

{ #category : #asserting }
MCRepositoryTest >> assertMissing: aVersionInfo [
	self assert: (repository versionWithInfo: aVersionInfo) isNil
]

{ #category : #asserting }
MCRepositoryTest >> assertVersionInfos: aCollection [
	self assert: repository allVersionInfos asSet equals: aCollection asSet
]

{ #category : #'tests-input data' }
MCRepositoryTest >> homeDirectoryUrl [
	^ FileLocator home asFileReference asZnUrl
]

{ #category : #actions }
MCRepositoryTest >> saveSnapshot1 [
	^ self saveSnapshot: self snapshot1 named: 'rev1'
]

{ #category : #actions }
MCRepositoryTest >> saveSnapshot2 [
	^ self saveSnapshot: self snapshot2 named: 'rev2'
]

{ #category : #actions }
MCRepositoryTest >> saveSnapshot: aSnapshot named: aString [
	| version |
	version := self versionWithSnapshot: aSnapshot name: aString.
	repository storeVersion: version.
	^ version info
	
]

{ #category : #building }
MCRepositoryTest >> snapshot1 [
	^ (MCSnapshot fromDefinitions: (Array with: (MCOrganizationDefinition categories: #('y'))))
]

{ #category : #building }
MCRepositoryTest >> snapshot2 [
	^ (MCSnapshot fromDefinitions: (Array with: (MCOrganizationDefinition categories: #('x'))))
]

{ #category : #accessing }
MCRepositoryTest >> snapshotAt: aVersionInfo [
	^ (repository versionWithInfo: aVersionInfo) snapshot
]

{ #category : #tests }
MCRepositoryTest >> testAddAndLoad [
	| node |
	node := self addVersionWithSnapshot: self snapshot1 name: 'rev1'.
	self assert: (self snapshotAt: node) equals: self snapshot1
]

{ #category : #tests }
MCRepositoryTest >> testCanCreateFileRepositoryFromUrl [
	repository := MCRepository fromUrl: 'file:///tmp'.
	self assert: (repository isKindOf: MCFileBasedRepository) 
]

{ #category : #tests }
MCRepositoryTest >> testCanCreateFiletreeRepositoryFromUrl [
	repository := MCRepository fromUrl: (self homeDirectoryUrl scheme: 'filetree') asString.
	self assert: (repository isKindOf: MCFileTreeRepository) 
]

{ #category : #tests }
MCRepositoryTest >> testCanCreateFiletreeRepositoryFromUrlWithRelativePath [
	repository := MCRepository fromUrl: 'filetree://.' asUrl.
	self assert: (repository isKindOf: MCFileTreeRepository) 
]

{ #category : #tests }
MCRepositoryTest >> testCanCreateFtpRepositoryFromUrl [
	repository := MCRepository fromUrl: 'ftp://ftp.example.com/examples'.
	self assert: (repository isKindOf: MCFtpRepository) 
]

{ #category : #tests }
MCRepositoryTest >> testCanCreateGitHubRepositoryFromUrl [
	repository := MCRepository fromUrl: 'github://dalehenrich/MetacelloRepository:master/monticello/repos/itory/path'.
	self assert: (repository isKindOf: MCGitHubRepository) 
]

{ #category : #tests }
MCRepositoryTest >> testCanCreateHttpRepositoryFromUrl [
	repository := MCRepository fromUrl: 'http://mydomain.com'.
	self assert: (repository isKindOf: MCHttpRepository) 
]

{ #category : #tests }
MCRepositoryTest >> testCanCreateHttpsRepositoryFromUrl [
	repository := MCRepository fromUrl: 'https://mydomain.com'.
	self assert: (repository isKindOf: MCHttpRepository) 
]

{ #category : #tests }
MCRepositoryTest >> testCannotCreateDictionaryRepositoryFromUrl [
	self
		should: [ MCRepository fromUrl: 'dict://foo' ]
		raise: MCRepositoryError
]

{ #category : #tests }
MCRepositoryTest >> testIncludesName [
	self deny: (repository includesVersionNamed: 'MonticelloTest-xxx.1-rev1').
	self saveSnapshot1.
	self assert: (repository includesVersionNamed: 'MonticelloTest-xxx.1-rev1').
	self deny: (repository includesVersionNamed: 'MonticelloTest-xxx.1-rev2').
	self saveSnapshot2.
	self assert:  (repository includesVersionNamed: 'MonticelloTest-xxx.1-rev2').
]

{ #category : #tests }
MCRepositoryTest >> testLoadMissingNode [
	| node |
	node := MCVersionInfo new.
	self assertMissing: node
]

{ #category : #tests }
MCRepositoryTest >> testStoreAndLoad [
	| node node2 |
	node := self saveSnapshot1.
	node2 := self saveSnapshot2.
	self assert: (self snapshotAt: node) equals: self snapshot1.
	self assert: (self snapshotAt: node2) equals: self snapshot2
]

{ #category : #building }
MCRepositoryTest >> versionWithSnapshot: aSnapshot name: aString [
	| info |
	info := self mockVersionInfo: aString. 
	^ MCVersion 
		package: (MCPackage new name: aString)
		info: info
		snapshot: aSnapshot
]
