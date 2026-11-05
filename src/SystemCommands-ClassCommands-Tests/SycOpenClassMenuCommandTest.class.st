"
Tests for the Commander2 submenu opened by SycOpenClassMenuCommand.
"
Class {
	#name : 'SycOpenClassMenuCommandTest',
	#superclass : 'SycAbstractCommandTest',
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycOpenClassMenuCommandTest >> testAddClyCommandsToMenuRendersGroupCommands [

	| group menu opener |
	opener := SycOpenClassMenuCommand new
		          context: self contextOnPoint;
		          yourself.

	group := ClyCommandGroupBuilder buildFor: SycClassCmCommand context: self contextOnPoint.

	menu := MenuMorph new.
	opener addClyCommandsToMenu: menu.

	self assert: menu items size equals: group commands size
]
