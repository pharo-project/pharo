Class {
	#name : 'SycMethodExtraCmCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-MethodCommands-Tests-Tests',
	#package : 'SystemCommands-MethodCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'helpers' }
SycMethodExtraCmCommandTest >> contextOnPoint [

	^ SycMockMethodContext new
		  selectedMethods: { Point >> #x };
		  yourself
]

{ #category : 'tests' }
SycMethodExtraCmCommandTest >> testGroupContainsExtraCommands [

	| group |
	group := ClyCommandGroupBuilder buildFor: SycMethodExtraCmCommand context: self contextOnPoint.
	self denyEmpty: group commands
]
