Class {
	#name : 'SycClassExtraCmCommandTest',
	#superclass : 'SycAbstractCommandTest',
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycClassExtraCmCommandTest >> testGroupContainsExtraCommands [

	| group |
	group := ClyCommandGroupBuilder buildFor: SycClassExtraCmCommand context: self contextOnPoint.
	self denyEmpty: group commands
]
