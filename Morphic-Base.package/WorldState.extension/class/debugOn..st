*Morphic-Base
debugOn: aBuilder
	<worldMenu>
	(aBuilder item: #Debugging)
		order: 3.5;
		icon: #smallDebug asIcon;
		with: [ (aBuilder item: #'Remove all Breakpoints')
				order: 0;
				action: [ Breakpoint removeAll ].
			(aBuilder item: #'Enable all break/inspect once')
				order: 0;
				action: [ Halt resetOnce ].
			(aBuilder item: #'Reset Counters')
				order: 0;
				action: [ ExecutionCounter resetAll ]	
				 ]