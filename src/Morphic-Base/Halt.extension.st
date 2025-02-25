Extension { #name : 'Halt' }

{ #category : '*Morphic-Base' }
Halt class >> resetOnceMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Enable all break/inspect once')
		parent: #Breakpoints;
		order: 99;
		help: 'Reset the break/inspect once. Once restarted, the first break once encountered will stop the execution.';
		action: [ Halt resetOnce ]
]
