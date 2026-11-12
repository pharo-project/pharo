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

	| entries |
	entries := ((ClyCommandRegistry default mapOf: ClyMethod) entriesOf: #contextMenu) select: [ :entry | entry menuGroup = SycMethodExtraMenuGroup ].
	self denyEmpty: entries.
	self assert: (entries allSatisfy: [ :entry |
				 (entry commandClass new
					  context: self contextOnPoint;
					  yourself) canBeExecuted ])
]
