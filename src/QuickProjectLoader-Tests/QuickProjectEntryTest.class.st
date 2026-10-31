"
Unit tests for QuickProjectEntry model.
"
Class {
	#name : 'QuickProjectEntryTest',
	#superclass : 'TestCase',
	#category : 'QuickProjectLoader-Tests',
	#package : 'QuickProjectLoader-Tests'
}

{ #category : 'tests' }
QuickProjectEntryTest >> testCreation [

	| entry |
	entry := QuickProjectEntry
		name: 'Roassal'
		repositoryUrl: 'github://ObjectProfile/Roassal/src'
		baseline: 'Roassal'.

	self assert: entry name equals: 'Roassal'.
	self assert: entry repositoryUrl equals: 'github://ObjectProfile/Roassal/src'.
	self assert: entry baselineName equals: 'Roassal'.
	self assert: entry loadCount equals: 0.
	self assert: entry useIncomingOnConflict.
	self assert: entry groups isEmpty
]

{ #category : 'tests' }
QuickProjectEntryTest >> testCreationWithoutBaseline [

	| entry |
	entry := QuickProjectEntry
		name: 'MyRepo'
		repositoryUrl: 'github://owner/repo'.

	self assert: entry name equals: 'MyRepo'.
	self assert: entry repositoryUrl equals: 'github://owner/repo'.
	self assert: entry baselineName isEmpty
]

{ #category : 'tests' }
QuickProjectEntryTest >> testDefaultGroups [

	| entry |
	entry := QuickProjectEntry new.
	self assert: entry groups equals: #()
]

{ #category : 'tests' }
QuickProjectEntryTest >> testEquality [

	| entry1 entry2 entry3 |
	entry1 := QuickProjectEntry
		name: 'Project A'
		repositoryUrl: 'github://owner/repo/src'
		baseline: 'MyProject'.
	entry2 := QuickProjectEntry
		name: 'Project A Modified Name'
		repositoryUrl: 'github://owner/repo/src'
		baseline: 'MyProject'.
	entry3 := QuickProjectEntry
		name: 'Project B'
		repositoryUrl: 'github://owner/other/src'
		baseline: 'OtherProject'.

	self assert: entry1 equals: entry2.
	self assert: entry1 hash equals: entry2 hash.
	self deny: entry1 = entry3
]

{ #category : 'tests' }
QuickProjectEntryTest >> testIncrementLoadCount [

	| entry |
	entry := QuickProjectEntry
		name: 'Test'
		repositoryUrl: 'github://a/b'
		baseline: 'Test'.

	self assert: entry loadCount equals: 0.
	entry incrementLoadCount.
	self assert: entry loadCount equals: 1.
	entry incrementLoadCount.
	self assert: entry loadCount equals: 2
]

{ #category : 'tests' }
QuickProjectEntryTest >> testMetacelloLoadScript [

	| entry script |
	entry := QuickProjectEntry
		name: 'Roassal'
		repositoryUrl: 'github://ObjectProfile/Roassal/src'
		baseline: 'Roassal'.

	script := entry metacelloLoadScript.
	self assert: (script includesSubstring: 'repository: ''github://ObjectProfile/Roassal/src''').
	self assert: (script includesSubstring: 'baseline: ''Roassal''').
	self assert: (script includesSubstring: 'onConflictUseIncoming').
	self assert: (script includesSubstring: 'load.')
]

{ #category : 'tests' }
QuickProjectEntryTest >> testMetacelloLoadScriptWithGroups [

	| entry script |
	entry := QuickProjectEntry
		name: 'Roassal'
		repositoryUrl: 'github://ObjectProfile/Roassal/src'
		baseline: 'Roassal'
		groups: #('Core' 'Tests').

	script := entry metacelloLoadScript.
	self assert: (script includesSubstring: 'load: #(''Core'' ''Tests'')')
]

{ #category : 'tests' }
QuickProjectEntryTest >> testMetacelloLoadScriptWithoutBaseline [

	| entry script |
	entry := QuickProjectEntry
		name: 'MyRepo'
		repositoryUrl: 'github://owner/repo'.

	script := entry metacelloLoadScript.
	self assert: (script includesSubstring: 'baseline: ''MyRepo''').
	self assert: (script includesSubstring: 'onConflictUseIncoming')
]

{ #category : 'tests' }
QuickProjectEntryTest >> testPrintOn [

	| entry |
	entry := QuickProjectEntry
		name: 'MyProject'
		repositoryUrl: 'github://a/b'
		baseline: 'MyProject'.
	entry incrementLoadCount.

	self assert: entry printString equals: 'MyProject (MyProject) - loaded 1 times'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testStonSerialization [

	| entry stonString restored |
	entry := QuickProjectEntry
		name: 'Roassal'
		repositoryUrl: 'github://ObjectProfile/Roassal/src'
		baseline: 'Roassal'
		groups: #('Full').
	entry useIncomingOnConflict: true.
	entry incrementLoadCount.

	stonString := STON toStringPretty: entry.
	restored := STON fromString: stonString.

	self assert: restored name equals: 'Roassal'.
	self assert: restored repositoryUrl equals: 'github://ObjectProfile/Roassal/src'.
	self assert: restored baselineName equals: 'Roassal'.
	self assert: restored groups equals: #('Full').
	self assert: restored loadCount equals: 1.
	self assert: restored useIncomingOnConflict.
	self assert: restored equals: entry
]

{ #category : 'tests' }
QuickProjectEntryTest >> testBaselineFallsBackOnName [

	| entry |
	entry := QuickProjectEntry name: 'Ume' repositoryUrl: 'github://FedeLoch/Ume'.

	self assert: entry effectiveBaselineName equals: 'Ume'.
	self assert: (entry metacelloLoadScript includesSubstring: 'baseline: ''Ume''')
]

{ #category : 'tests' }
QuickProjectEntryTest >> testBaselinePrefixIsStripped [

	self
		assert: (QuickProjectEntry name: 'X' repositoryUrl: '' baseline: 'BaselineOfFoo') effectiveBaselineName
		equals: 'Foo'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testCloneUrlsBecomeMetacelloUrls [

	#( 'git@github.com:FedeLoch/Ume.git' 'https://github.com/FedeLoch/Ume.git' 'https://github.com/FedeLoch/Ume' ) do: [ :url |
		self
			assert: (QuickProjectEntry name: 'Ume' repositoryUrl: url) metacelloRepositoryUrl
			equals: 'github://FedeLoch/Ume' ].
	self
		assert: (QuickProjectEntry name: 'X' repositoryUrl: 'github://a/b:main/src') metacelloRepositoryUrl
		equals: 'github://a/b:main/src'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testDisplayRepository [

	self
		assert: (QuickProjectEntry name: 'X' repositoryUrl: 'github://a/b:main/src') displayRepository
		equals: 'a/b'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testEqualEntriesHashEqually [

	self
		assert: (QuickProjectEntry name: 'A' repositoryUrl: 'github://a/b') hash
		equals: (QuickProjectEntry name: 'B' repositoryUrl: 'github://a/b') hash
]

{ #category : 'tests' }
QuickProjectEntryTest >> testRecordLoadInSessionKeepsLastThirty [

	| entry start |
	entry := QuickProjectEntry name: 'A' repositoryUrl: 'github://a/b'.
	start := DateAndTime now.
	entry recordLoadInSession: start.
	self assert: entry loadedInSessions size equals: 1.

	1 to: 40 do: [ :i | entry recordLoadInSession: start + i seconds ].

	self assert: entry loadedInSessions size equals: 30.
	self assert: entry loadedInSessions last equals: start + 40 seconds
]

{ #category : 'tests' }
QuickProjectEntryTest >> testBranchGoesInItsPlaceInTheUrl [

	| entry |
	entry := QuickProjectEntry name: 'A' repositoryUrl: 'github://owner/repo/src'.
	entry branch: 'dev'.

	self assert: entry metacelloRepositoryUrl equals: 'github://owner/repo:dev/src'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testBranchOnCloneUrl [

	| entry |
	entry := QuickProjectEntry name: 'Ume' repositoryUrl: 'git@github.com:FedeLoch/Ume.git'.
	entry branch: 'main'.

	self assert: entry metacelloRepositoryUrl equals: 'github://FedeLoch/Ume:main'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testBranchWinsOverTheOneInTheUrl [

	| entry |
	entry := QuickProjectEntry name: 'A' repositoryUrl: 'github://owner/repo:old/src'.
	entry branch: 'new'.

	self assert: entry metacelloRepositoryUrl equals: 'github://owner/repo:new/src'.
	self assert: entry effectiveBranch equals: 'new'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testDefaultBranchIsResolvedOnlyWithoutABranch [

	| entry |
	entry := QuickProjectLoadRecorder new
		name: 'A';
		repositoryUrl: 'github://owner/repo';
		defaultBranch: 'main';
		yourself.
	entry resolveDefaultBranch.
	self assert: entry metacelloRepositoryUrl equals: 'github://owner/repo:main'.

	entry
		repositoryUrl: 'github://owner/repo:dev';
		branch: ''.
	entry resolveDefaultBranch.
	self assert: entry metacelloRepositoryUrl equals: 'github://owner/repo:dev'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testDefaultBranchLookupFailureKeepsTheUrl [

	| entry |
	entry := QuickProjectLoadRecorder new
		name: 'A';
		repositoryUrl: 'github://owner/repo';
		yourself.

	entry resolveDefaultBranch.

	self assert: entry metacelloRepositoryUrl equals: 'github://owner/repo'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testDefaultBranchParsing [

	self assert: (QuickProjectEntry defaultBranchIn: '{"name":"Ume","default_branch":"main"}') equals: 'main'.
	self assert: (QuickProjectEntry defaultBranchIn: '{"message":"Not Found"}') isNil
]

{ #category : 'tests' }
QuickProjectEntryTest >> testDisplayBranch [

	| entry |
	entry := QuickProjectEntry name: 'A' repositoryUrl: 'github://owner/repo/src'.
	self assert: entry displayBranch equals: 'default'.

	entry branch: 'main'.
	self assert: entry displayBranch equals: 'main'.
	self
		assert: (QuickProjectEntry name: 'A' repositoryUrl: 'github://owner/repo:dev') displayBranch
		equals: 'dev'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testOnlyGitHubIsAskedForADefaultBranch [

	self assert: (QuickProjectEntry name: 'A' repositoryUrl: 'git@github.com:a/b.git') isGitHubRepository.
	self deny: (QuickProjectEntry name: 'A' repositoryUrl: 'gitlab://a/b') isGitHubRepository.
	self assert: (QuickProjectEntry name: 'A' repositoryUrl: 'gitlab://a/b') fetchDefaultBranch isNil
]

{ #category : 'tests' }
QuickProjectEntryTest >> testBranchSurvivesTheFile [

	| entry |
	entry := QuickProjectEntry name: 'A' repositoryUrl: 'github://a/b'.
	entry branch: 'main'.

	self assert: (STON fromString: (STON toString: entry)) branch equals: 'main'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testBranchNamesParsing [

	self
		assert: (QuickProjectEntry branchNamesIn: '[{"name":"main"},{"name":"dev"}]') asArray
		equals: #( 'main' 'dev' ).
	self assert: (QuickProjectEntry branchNamesIn: '{"message":"Not Found"}') isEmpty
]

{ #category : 'tests' }
QuickProjectEntryTest >> testOnlyGitHubIsAskedForBranches [

	self assert: (QuickProjectEntry name: 'A' repositoryUrl: 'gitlab://a/b') fetchBranches isEmpty
]
