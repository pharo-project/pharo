"
A LtCommandsRegistryTest is a test class for testing the behavior of LtCommandsRegistry
"
Class {
	#name : #CmCommandsGroupTest,
	#superclass : #TestCase,
	#instVars : [
		'firstCommand',
		'subCommand1',
		'group',
		'subGroup1',
		'subGroup2'
	],
	#category : #'Commander2-Tests'
}

{ #category : #running }
CmCommandsGroupTest >> setUp [
	super setUp.
	group := CmCommandGroup named: 'main group'.
	firstCommand := CmBlockCommand new
							name: 'first command';
							block: [ :x | x ];
							context: [ 1 ];
							yourself.
	
	
	subCommand1 :=  CmBlockCommand new
							name: 'sub command 1';
							block: [ :x | x ];
							context: [ 11 ];
							yourself.
	
	subGroup1 := (CmCommandGroup named: 'sub group 1')
							register: subCommand1;
							yourself.
	
	subGroup2 := CmCommandGroup named: 'sub group 2'. "Empty on purpose."
	
	group
		register: firstCommand;
		register: subGroup1;
		register: subGroup2
]

{ #category : #test }
CmCommandsGroupTest >> testAllCommands [
	self assertCollection: group allCommands equals: { firstCommand . subCommand1 } asOrderedCollection
]

{ #category : #test }
CmCommandsGroupTest >> testCommandOrGroupNamed [
	self assert: (group commandOrGroupNamed: 'first command') equals: firstCommand.
	self assert: (group commandOrGroupNamed: 'sub group 1') equals: subGroup1.
	self assert: (group commandOrGroupNamed: 'sub group 2') equals: subGroup2.	
	
	self should: [ group commandOrGroupNamed: 'unknown' ] raise: NotFound.
	
	self assert: ((group commandOrGroupNamed: 'sub group 1') commandOrGroupNamed: 'sub command 1') equals: subCommand1.
]

{ #category : #test }
CmCommandsGroupTest >> testCommands [
	self assertCollection: group commands equals: { firstCommand } asOrderedCollection
]

{ #category : #test }
CmCommandsGroupTest >> testEntries [
	self assert: (group entries allSatisfy: [ :e | e isCommand or: [ e isGroup ] ]).
	self assertCollection: (group entries) equals: { firstCommand . subGroup1 . subGroup2 } asOrderedCollection
]

{ #category : #test }
CmCommandsGroupTest >> testGroups [
	self assertCollection: group groups equals: { subGroup1. subGroup2 } asOrderedCollection
]

{ #category : #test }
CmCommandsGroupTest >> testHasCommand [
	self deny: (group hasCommand: CmCommand new).
	
	self assert: (group hasCommand: firstCommand)
]

{ #category : #test }
CmCommandsGroupTest >> testHasEntryNamed [
	self deny: (group hasEntryNamed: 'nope').
	
	self assert: (group hasEntryNamed: 'first command').
	
	self deny: (group hasEntryNamed: 'sub command 1').
]

{ #category : #test }
CmCommandsGroupTest >> testInitialize [
	| newGroup |
	newGroup := CmCommandGroup new.
	
	self assert: newGroup name equals: CmCommandGroup defaultName.
	self assert: newGroup entries isEmpty
]

{ #category : #test }
CmCommandsGroupTest >> testRegisterCommandWithContext [
	| secondCommand |
	self deny: (group hasEntryNamed: 'second command').
	
	secondCommand := CmBlockCommand new 
							name: 'second command';
							block: [ :x | x ];
							context: 42;
							yourself.
	group register: secondCommand.
	
	self assert: (group hasEntryNamed: 'second command').
	self assert: (group commandOrGroupNamed: 'second command') equals: secondCommand.
	self assert: group entries last context equals: 42
]

{ #category : #test }
CmCommandsGroupTest >> testRegisterCommandWithContextBlock [
	| secondCommand secondCommandWithSameName |
	self deny: (group hasEntryNamed: 'second command').
	
	secondCommand := CmBlockCommand new 
							name: 'second command';
							block: [ :x | x ];
							context: [ 42 ];
							yourself.
	group register: secondCommand.
	
	self assert: (group hasEntryNamed: 'second command').
	self assert: (group commandOrGroupNamed: 'second command') equals: secondCommand.
	
	secondCommandWithSameName := CmBlockCommand new 
											name: 'second command';
											block: [ :x | x ];
											context: [ ];
											yourself.
	
	self
		should: [ group register: secondCommandWithSameName]
		raise: CmDuplicatedEntryName
		withExceptionDo: [ :error |
			self assert: error entryName equals: 'second command' ]
]

{ #category : #test }
CmCommandsGroupTest >> testRegisterSubRegister [
	| subGroup3 |
	self deny: (group hasEntryNamed: 'sub group 3').
	
	subGroup3 := CmCommandGroup named: 'sub group 3'.
	group register: subGroup3.
	
	self assert: (group hasEntryNamed: 'sub group 3').
	self assert: (group commandOrGroupNamed: 'sub group 3') equals: subGroup3.
	
	self
		should: [ group register: (CmCommandGroup named: 'sub group 3') ]
		raise: CmDuplicatedEntryName
		withExceptionDo: [ :error |
			self assert: error entryName equals: 'sub group 3' ]
]

{ #category : #test }
CmCommandsGroupTest >> testUnregister [
	self assert: (group hasEntryNamed: 'first command').
	group unregister: firstCommand.
	self deny: (group hasEntryNamed: 'first command').
	
	self assert: (group hasEntryNamed: 'sub group 1').
	group unregister: subGroup1.
	self deny: (group hasEntryNamed: 'sub group 1').
]
