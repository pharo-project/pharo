Class {
	#name : 'SycMethodExtraCmCommandTest',
	#superclass : 'SycAbstractCommandTest',
	#category : 'SystemCommands-MethodCommands-Tests-Tests',
	#package : 'SystemCommands-MethodCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycMethodExtraCmCommandTest >> testGroupContainsExtraCommands [

	| group |
	group := ClyCommandGroupBuilder buildFor: SycMethodExtraCmCommand context: self contextOnPoint.
	self denyEmpty: group commands
]
