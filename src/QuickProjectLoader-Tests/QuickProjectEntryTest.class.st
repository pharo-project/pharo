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
	self assert: entry groups isEmpty
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
		name: 'Project A'
		repositoryUrl: 'github://owner/other/src'
		baseline: 'MyProject'.

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
QuickProjectEntryTest >> testPrintOn [

	| entry |
	entry := QuickProjectEntry
		name: 'MyProject'
		repositoryUrl: 'github://a/b'
		baseline: 'MyProject'.
	entry incrementLoadCount.

	self assert: entry printString equals: 'MyProject (MyProject - loaded 1 times)'
]

{ #category : 'tests' }
QuickProjectEntryTest >> testStonSerialization [

	| entry stonString restored |
	entry := QuickProjectEntry
		name: 'Roassal'
		repositoryUrl: 'github://ObjectProfile/Roassal/src'
		baseline: 'Roassal'
		groups: #('Full').
	entry incrementLoadCount.

	stonString := STON toStringPretty: entry.
	restored := STON fromString: stonString.

	self assert: restored name equals: 'Roassal'.
	self assert: restored repositoryUrl equals: 'github://ObjectProfile/Roassal/src'.
	self assert: restored baselineName equals: 'Roassal'.
	self assert: restored groups equals: #('Full').
	self assert: restored loadCount equals: 1.
	self assert: restored equals: entry
]
