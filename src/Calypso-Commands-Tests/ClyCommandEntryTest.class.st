"
Tests for ClyCommandEntry, on the commands declared by ClyMockToolContext and ClyMockToolSubContext.
"
Class {
	#name : 'ClyCommandEntryTest',
	#superclass : 'TestCase',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'helpers' }
ClyCommandEntryTest >> contextMenuEntryOf: aCommandClass on: anAnchorClass [

	^ ((ClyCommandMap forAnchor: anAnchorClass) entriesOf: #contextMenu) detect: [ :entry | entry commandClass == aCommandClass ]
]

{ #category : 'tests' }
ClyCommandEntryTest >> testActivatorRunsTheCommandInTheContext [

	| context activator |
	context := ClyMockToolContext new.
	activator := (self contextMenuEntryOf: ClyMockRegisteredFirstCommand on: ClyMockToolContext) newActivatorFor: context.
	self assert: activator command class identicalTo: ClyMockRegisteredFirstCommand.
	self assert: activator context identicalTo: context.
	self assert: activator menuItemOrder equals: 20.
	self assert: activator menuItemName equals: 'First'
]

{ #category : 'tests' }
ClyCommandEntryTest >> testDeclaredLabelIsUsed [

	| entry |
	entry := self contextMenuEntryOf: ClyMockRegisteredSecondCommand on: ClyMockToolSubContext.
	self assert: (entry menuItemNameFor: ClyMockRegisteredSecondCommand new) equals: 'Second from the subcontext'
]

{ #category : 'tests' }
ClyCommandEntryTest >> testIsActiveInContextsDescribedByItsAnchor [

	| entry |
	entry := self contextMenuEntryOf: ClyMockRegisteredFirstCommand on: ClyMockToolContext.
	self assert: (entry isActiveInContext: ClyMockToolContext new).
	self assert: (entry isActiveInContext: ClyMockToolSubContext new).
	entry := self contextMenuEntryOf: ClyMockRegisteredSecondCommand on: ClyMockToolSubContext.
	self deny: (entry isActiveInContext: ClyMockToolContext new)
]

{ #category : 'tests' }
ClyCommandEntryTest >> testLabelIsAskedToTheCommandWhenNotDeclared [

	| entry |
	entry := self contextMenuEntryOf: ClyMockRegisteredSecondCommand on: ClyMockToolContext.
	self assert: (entry menuItemNameFor: ClyMockRegisteredSecondCommand new) equals: 'Second'
]

{ #category : 'tests' }
ClyCommandEntryTest >> testOrderIsAskedToTheCommandWhenNotDeclared [

	| entry command |
	entry := self contextMenuEntryOf: ClyMockRegisteredSecondCommand on: ClyMockToolContext.
	command := ClyMockRegisteredSecondCommand new
		           context: ClyMockToolContext new;
		           yourself.
	self assert: (entry menuItemOrderFor: command) equals: 500
]
