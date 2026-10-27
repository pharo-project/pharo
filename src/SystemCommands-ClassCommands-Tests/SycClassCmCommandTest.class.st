"
Tests for the Commander2 command group built from SycClassCmCommand.
"
Class {
	#name : 'SycClassCmCommandTest',
	#superclass : 'SycAbstractCommandTest',
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycClassCmCommandTest >> testGroupContainsCommands [

	| group |
	group := ClyCommandGroupBuilder buildFor: SycClassCmCommand context: self contextOnPoint.

	self denyEmpty: group commands.
	self assert: (group commands noneSatisfy: [ :command | command class isAbstract ])
]
