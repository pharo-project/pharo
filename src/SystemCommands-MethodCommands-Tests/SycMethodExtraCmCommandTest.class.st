Class {
	#name : 'SycMethodExtraCmCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-MethodCommands-Tests-Tests',
	#package : 'SystemCommands-MethodCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycMethodExtraCmCommandTest >> testGroupContainsExtraCommands [

	| group |
	group := ClyCommandGroupBuilder buildFor: SycMethodExtraCmCommand context: SycMockMethodContext new.
	self denyEmpty: group commands
]
