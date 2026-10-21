Class {
	#name : 'SycClassExtraCmCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycClassExtraCmCommandTest >> testGroupContainsExtraCommands [

	| group |
	group := ClyCommandGroupBuilder buildFor: SycClassExtraCmCommand context: SycMockClassContext new.
	self denyEmpty: group commands
]
