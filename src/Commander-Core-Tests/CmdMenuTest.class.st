Class {
	#name : #CmdMenuTest,
	#superclass : #TestCase,
	#category : #'Commander-Core-Tests'
}

{ #category : #tests }
CmdMenuTest >> buildMenuInContext: aToolContext [
	| menu |
	menu := CmdMenu  
		activatedBy: CmdMenuCommandActivationExample.
	
	menu buildInContext: aToolContext.
	
	^menu
]

{ #category : #tests }
CmdMenuTest >> testCommandItemShouldContainPreparedCommandInstance [
	| menu command context containingItems |
	context := CmdToolContextStub1 new.
	menu := self buildMenuInContext: context.
	containingItems := menu itemsWithCommandLike: CmdCommandExampleInRootMenu.
	self assert: containingItems last isCommandItem.
	
	command := containingItems last command.
	self assert: command class equals: CmdCommandExampleInRootMenu.
	self assert: command savedParametersContext equals: context.
]

{ #category : #tests }
CmdMenuTest >> testWhenCommandShouldBeInDeepChildGroup [
	| menu containingItems parents |
	menu := self buildMenuInContext: CmdToolContextStub2 new.
	
	containingItems := menu itemsWithCommandLike: CmdCommandExampleInDeepMenuGroup.
	self assert: containingItems size equals: 2. "group and commandItem"
	self assert: containingItems last isCommandItem.
	
	parents := containingItems last pathToRoot collect: #class.
	self assert: parents equals: {CmdRootMenuGroup. CmdMenuGroupExample2. CmdDeepChildMenuGroupStub }
]

{ #category : #tests }
CmdMenuTest >> testWhenCommandShouldBeInMultipleGroups [
	| menu containingGroups |
	menu := self buildMenuInContext: CmdToolContextStub1 new.
	
	containingGroups := menu groupsWithCommandLike: CmdCommandExampleInMultipleMenuGroups.
	self 
		assert: (containingGroups collect: #class as: Set)
		equals: {CmdMenuGroupExample1. CmdMenuGroupExample2. CmdRootMenuGroup } asSet
]

{ #category : #tests }
CmdMenuTest >> testWhenCommandShouldBeInRootMenu [
	| menu containingItems |
	menu := self buildMenuInContext: CmdToolContextStub1 new.
	
	containingItems := menu groupsWithCommandLike: CmdCommandExampleInRootMenu.
	self assert: containingItems size equals: 1. "group and commandItem"
	self assert: containingItems first equals: menu rootGroup
]

{ #category : #tests }
CmdMenuTest >> testWhenGivenContextIsUnused [
	| menu |
	menu := self buildMenuInContext: CmdUnusedToolContextStub new.
	
	self assert: menu rootGroup size equals: 0
]
