Extension { #name : 'ClyClass' }

{ #category : '*SystemCommands-ClassCommands' }
ClyClass class >> systemCommandsClassCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu
		add: SycOpenExtraInClassMenuCommand order: 10200;
		add: SycOpenScopesInClassMenuCommand order: 1.5.
	aCommandMap shortcuts add: SycCmDuplicateClassCommand key: $d meta
]
