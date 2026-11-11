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

{ #category : 'tests' }
ClyCommandMapTest >> testShortcutEntryHasItsKeyCombination [

	| entries |
	entries := (ClyCommandMap forAnchor: ClyMockToolContext) entriesOf: #shortcuts.
	self assert: entries size equals: 1.
	self assert: entries first commandClass identicalTo: ClyMockRegisteredFirstCommand.
	self assert: entries first keyCombination equals: $x meta
]
