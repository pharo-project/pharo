"
Tests for ClyCommandMap and ClyCommandInteraction, on the commands declared by ClyMockToolContext and ClyMockToolSubContext.
"
Class {
	#name : 'ClyCommandMapTest',
	#superclass : 'TestCase',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'tests' }
ClyCommandMapTest >> testAddAllConcreteSubclassesAddsTheFamily [

	| map |
	map := ClyCommandMap forAnchor: ClyMockToolSubContext.
	self assert: ((map entriesOf: #toolbar) collect: [ :entry | entry commandClass ]) asArray equals: {
			ClyMockRegisteredFirstCommand.
			ClyMockRegisteredSecondCommand }
]

{ #category : 'tests' }
ClyCommandMapTest >> testDeclarationsOfSuperclassAnchorAreNotCollected [

	| map |
	map := ClyCommandMap forAnchor: ClyMockToolSubContext.
	self assert: (map entriesOf: #contextMenu) size equals: 1.
	self assert: (map entriesOf: #shortcuts) isEmpty
]

{ #category : 'tests' }
ClyCommandMapTest >> testDropEntryIsActiveInContextsOfItsDropAnchor [

	| map entry |
	map := ClyCommandMap forAnchor: ClyMockToolSubContext.
	entry := map dragAndDrop add: ClyMockRegisteredFirstCommand dropIn: ClyMockToolSubContext.
	self assert: (entry isDropActiveInContext: ClyMockToolSubContext new).
	self deny: (entry isDropActiveInContext: ClyMockToolContext new)
]

{ #category : 'tests' }
ClyCommandMapTest >> testEntriesKnowTheirDeclaringMethod [

	| map |
	map := ClyCommandMap forAnchor: ClyMockToolContext.
	self assert: map allEntries size equals: 3.
	self assert: (map allEntries allSatisfy: [ :entry | entry declaringMethod == (ClyMockToolContext class >> #mockCommandsIn:) ])
]

{ #category : 'tests - overrides' }
ClyCommandMapTest >> testEntriesWithoutOverrideHaveNoOverridingMethod [

	self assert: ((ClyCommandMap forAnchor: ClyMockToolContext) allEntries allSatisfy: [ :entry | entry overridingMethod isNil ])
]

{ #category : 'tests' }
ClyCommandMapTest >> testGroupViewAddsToTheSameInteraction [

	| map |
	map := ClyCommandMap forAnchor: ClyMockToolContext.
	self assert: ((map entriesOf: #contextMenu) collect: [ :entry | entry commandClass -> entry group ]) asArray equals: {
			(ClyMockRegisteredFirstCommand -> ClyRootMenuGroup).
			(ClyMockRegisteredSecondCommand -> ClyExtraMenuGroup) }
]

{ #category : 'tests' }
ClyCommandMapTest >> testInteractionWithoutDeclarationHasNoEntries [

	self assertEmpty: ((ClyCommandMap forAnchor: ClyMockToolContext) entriesOf: #doubleClick)
]

{ #category : 'tests - overrides' }
ClyCommandMapTest >> testRebindChangesTheKeyOfTheShortcut [

	| map shortcut |
	map := ClyCommandMap forAnchor: ClyMockOverridingToolContext.
	shortcut := (map entriesOf: #shortcuts) detect: [ :entry | entry annotates: ClyMockRegisteredFirstCommand ].
	self assert: shortcut keyCombination equals: $d meta.
	self assert: shortcut declaringMethod identicalTo: ClyMockOverridingToolContext class >> #mockCommandsIn:.
	self assert: shortcut overridingMethod identicalTo: ClyMockOverridingToolContext class >> #earlyMockOverridesIn:
]

{ #category : 'tests - overrides' }
ClyCommandMapTest >> testRebindMenuOfChangesTheKeyOpeningTheGroup [

	| map shortcut |
	map := ClyCommandMap forAnchor: ClyMockOverridingToolContext.
	shortcut := (map entriesOf: #shortcuts) detect: [ :entry | entry openedGroup = ClyExtraMenuGroup ].
	self assert: shortcut keyCombination equals: $e meta
]

{ #category : 'tests - overrides' }
ClyCommandMapTest >> testRebindOutsideShortcutsFails [

	self should: [ ClyCommandMap new contextMenu rebind: ClyMockRegisteredFirstCommand to: $d meta ] raise: Error
]

{ #category : 'tests - overrides' }
ClyCommandMapTest >> testRemoveHidesTheCommandInEveryGroup [

	| map |
	map := ClyCommandMap forAnchor: ClyMockOverridingToolContext.
	self assert: ((map entriesOf: #contextMenu) collect: [ :entry | entry commandClass ]) asArray equals: { ClyMockRegisteredFirstCommand }
]

{ #category : 'tests - overrides' }
ClyCommandMapTest >> testRemoveMenuOfRemovesTheEntryOpeningTheGroup [

	| map |
	map := ClyCommandMap forAnchor: ClyMockOverridingToolContext.
	self assert: ((map entriesOf: #tabActions) collect: [ :entry | entry commandClass ]) asArray equals: { ClyMockRegisteredFirstCommand }
]

{ #category : 'tests - overrides' }
ClyCommandMapTest >> testRemoveOfAShortcut [

	| map |
	map := ClyCommandMap forAnchor: ClyMockOverridingToolContext.
	self deny: ((map entriesOf: #shortcuts) anySatisfy: [ :entry | entry annotates: ClyMockRegisteredSecondCommand ])
]

{ #category : 'tests - overrides' }
ClyCommandMapTest >> testRemoveOfAnUndeclaredCommandDoesNothing [

	| map |
	map := ClyCommandMap forAnchor: ClyMockOverridingToolContext.
	self assert: (map entriesOf: #contextMenu) size equals: 1.
	self deny: ((map entriesOf: #contextMenu) anySatisfy: [ :entry | entry annotates: ClyMockFirstCommand ])
]

{ #category : 'tests' }
ClyCommandMapTest >> testShortcutEntryHasItsKeyCombination [

	| entries |
	entries := (ClyCommandMap forAnchor: ClyMockToolContext) entriesOf: #shortcuts.
	self assert: entries size equals: 1.
	self assert: entries first commandClass identicalTo: ClyMockRegisteredFirstCommand.
	self assert: entries first keyCombination equals: $x meta
]

{ #category : 'tests - overrides' }
ClyCommandMapTest >> testShortcutsBrowserBrowsesTheOverridingMethod [

	| map |
	map := ClyCommandMap forAnchor: ClyMockOverridingToolContext.
	self assert: ((map entriesOf: #shortcuts) detect: [ :entry | entry annotates: ClyMockRegisteredFirstCommand ]) action identicalTo: ClyMockOverridingToolContext class >> #earlyMockOverridesIn:.
	self assert: ((map entriesOf: #shortcuts) detect: [ :entry | entry openedGroup = ClyExtraMenuGroup ]) action identicalTo: ClyMockOverridingToolContext class >> #earlyMockOverridesIn:
]
