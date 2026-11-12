Class {
	#name : 'SycClassExtraMenuGroupTest',
	#superclass : 'SycAbstractCommandTest',
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycClassExtraMenuGroupTest >> testGroupContainsExtraCommands [

	| entries |
	entries := ((ClyCommandRegistry default mapOf: ClyClass) entriesOf: #contextMenu) select: [ :entry | entry menuGroup = SycClassExtraMenuGroup ].
	self denyEmpty: entries.
	self assert: ((entries collect: [ :entry |
				  entry commandClass new
					  context: self contextOnPoint;
					  yourself ]) anySatisfy: [ :command | command canBeExecuted ])
]
