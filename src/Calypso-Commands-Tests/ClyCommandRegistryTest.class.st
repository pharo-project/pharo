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
ClyCommandRegistryTest >> testActivationsAreSortedByPriority [

	| map context |
	map := registry mapOf: ClyMockToolContext.
	(map doubleClick add: ClyMockRegisteredFirstCommand) priority: 1.
	(map doubleClick add: ClyMockRegisteredSecondCommand) priority: 10.
	(map tableIcons add: ClyMockRegisteredFirstCommand) priority: 1.
	(map tableIcons add: ClyMockRegisteredSecondCommand) priority: 10.
	context := ClyMockToolContext new.
	self assert: ((registry activationsOf: #doubleClick inContext: context) collect: [ :entry | entry commandClass ]) asArray equals: {
			ClyMockRegisteredSecondCommand.
			ClyMockRegisteredFirstCommand }.
	self assert: ((registry activationsOf: #tableIcons inContext: context) collect: [ :entry | entry commandClass ]) asArray equals: {
			ClyMockRegisteredFirstCommand.
			ClyMockRegisteredSecondCommand }
]

{ #category : 'tests' }
ClyCommandRegistryTest >> testActivationsIncludeRemainingClassAnnotations [

	| activations |
	activations := ClyCommandRegistry default activationsOf: #contextMenu inContext: ClyMockAnnotatedToolContext new.
	self assert: activations size equals: 1.
	self assert: activations first commandClass identicalTo: ClyMockAnnotatedCommand
]

{ #category : 'tests' }
ClyCommandRegistryTest >> testAddMenuOfDeclaresACommandOpeningTheGroup [

	| entry command |
	entry := (registry mapOf: ClyMockToolContext) shortcuts addMenuOf: ClyExtraMenuGroup key: $y meta.
	self assert: entry commandClass identicalTo: ClyOpenMenuGroupCommand.
	self assert: entry openedGroup identicalTo: ClyExtraMenuGroup.
	command := entry newCommandInContext: ClyMockToolContext new.
	self assert: command menuGroupClass identicalTo: ClyExtraMenuGroup.
	self assert: command name equals: ClyExtraMenuGroup new name
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
ClyCommandRegistryTest >> testPresentedGroupOfAMenuGroupClass [

	| context root group |
	context := ClyMockToolContext new.
	root := ClyCommandGroupBuilder new buildMenuOf: (registry activationsOf: #contextMenu inContext: context) context: context pureRoots: #(  ).
	group := root presentedGroupOf: ClyExtraMenuGroup.
	self assert: group descriptor class identicalTo: ClyExtraMenuGroup.
	self assert: (group entries collect: [ :entry | entry command class ]) asArray equals: { ClyMockRegisteredSecondCommand }.
	self assert: (root presentedGroupOf: ClyQueryMenuGroup) isNil.
	self assert: group toolContext identicalTo: context
]

{ #category : 'tests' }
ClyCommandRegistryTest >> testShortcutsOfACommandInContext [

	| shortcuts |
	shortcuts := registry shortcutsOf: ClyMockRegisteredFirstCommand inContext: ClyMockToolSubContext new.
	self assert: shortcuts size equals: 1.
	self assert: shortcuts first keyCombination equals: $x meta.
	self assertEmpty: (registry shortcutsOf: ClyMockRegisteredSecondCommand inContext: ClyMockToolSubContext new)
]

{ #category : 'tests' }
ClyCommandRegistryTest >> testShortcutsOpeningAGroup [

	| shortcuts |
	(registry mapOf: ClyMockToolContext) shortcuts addMenuOf: ClyExtraMenuGroup key: $y meta.
	shortcuts := registry shortcutsOpeningGroup: ClyExtraMenuGroup inContext: ClyMockToolSubContext new.
	self assert: shortcuts size equals: 1.
	self assert: shortcuts first keyCombination equals: $y meta.
	self assertEmpty: (registry shortcutsOpeningGroup: ClyRootMenuGroup inContext: ClyMockToolSubContext new)
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
