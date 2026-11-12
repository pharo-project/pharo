Class {
	#name : 'SycMethodExtraMenuGroupTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-MethodCommands-Tests-Tests',
	#package : 'SystemCommands-MethodCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'helpers' }
SycMethodExtraMenuGroupTest >> contextOnPoint [

	^ SycMockMethodContext new
		  selectedMethods: { Point >> #x };
		  yourself
]

{ #category : 'tests' }
SycMethodExtraMenuGroupTest >> testGroupContainsExtraCommands [

	| entries |
	entries := ((ClyCommandRegistry default mapOf: ClyMethod) entriesOf: #contextMenu) select: [ :entry | entry menuGroup = SycMethodExtraMenuGroup ].
	self denyEmpty: entries.
	self assert: (entries allSatisfy: [ :entry |
				 (entry commandClass new
					  context: self contextOnPoint;
					  yourself) canBeExecuted ])
]
