"
Tests for ClyCommandRegistry, on a registry limited to the mock anchors ClyMockToolContext and ClyMockToolSubContext.
"
Class {
	#name : 'ClyCommandRegistryTest',
	#superclass : 'TestCase',
	#instVars : [
		'registry'
	],
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'running' }
ClyCommandRegistryTest >> setUp [

	super setUp.
	registry := ClyCommandRegistry forAnchors: {
			            ClyMockToolContext.
			            ClyMockToolSubContext }
]

{ #category : 'tests' }
ClyCommandRegistryTest >> testDefaultRegistryFindsDeclaredAnchors [

	self assert: (ClyCommandRegistry default anchors includesAll: {
				 ClyMockToolContext.
				 ClyMockToolSubContext })
]

{ #category : 'tests' }
ClyCommandRegistryTest >> testDefaultRegistryIsResetWhenADeclarationIsAdded [

	| previous |
	previous := ClyCommandRegistry default.
	ClyMockToolSubContext class
		compile: 'temporaryCommandsIn: aCommandMap
	<calypsoCommands>'
		classified: 'commands'.
	[ self deny: ClyCommandRegistry default identicalTo: previous ] ensure: [ ClyMockToolSubContext class removeSelector: #temporaryCommandsIn: ]
]

{ #category : 'tests' }
ClyCommandRegistryTest >> testEntriesComeFromEveryAnchorDescribingTheContext [

	self assert: (registry entriesOf: #contextMenu inContext: ClyMockToolContext new) size equals: 2.
	self assert: (registry entriesOf: #contextMenu inContext: ClyMockToolSubContext new) size equals: 3
]

{ #category : 'tests' }
ClyCommandRegistryTest >> testMenuOfEntriesIsPresentedInGroups [

	| context root extraGroup |
	context := ClyMockToolSubContext new.
	root := ClyCommandGroupBuilder new buildMenuOf: (registry entriesOf: #contextMenu inContext: context) context: context pureRoots: #(  ).
	self assert: (root entries collect: [ :entry | entry dynamicName ]) asArray equals: #( 'First' 'Second from the subcontext' 'Extra' ).
	extraGroup := root entries last.
	self assert: extraGroup descriptor class identicalTo: ClyExtraMenuGroup.
	self assert: (extraGroup entries collect: [ :entry | entry dynamicName ]) asArray equals: #( 'Second' )
]

{ #category : 'tests' }
ClyCommandRegistryTest >> testToolbarOfEntriesIsPresentedInGroups [

	| context root |
	context := ClyMockToolSubContext new.
	root := ClyCommandGroupBuilder new buildToolbarOf: [ :toolContext | registry entriesOf: #toolbar inContext: toolContext ] contexts: { context }.
	self assert: root entries size equals: 1.
	self assert: (root entries first entries collect: [ :entry | entry command class ]) asArray equals: {
			ClyMockRegisteredSecondCommand.
			ClyMockRegisteredFirstCommand }
]
