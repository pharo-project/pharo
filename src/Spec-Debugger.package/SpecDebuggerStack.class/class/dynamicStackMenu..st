menu
dynamicStackMenu: aBuilder
	<contextMenu>
	<specDebuggerStackMenuNew>
	
	| target |
	target := aBuilder model.
	target currentContext ifNil: [ ^ target ].
	target currentContext class refactoringMethod: aBuilder.
	target currentContext class methodRefactoring: aBuilder.
		
	aBuilder model stack generateContextMenuDebuggingActions do: [ :anAction | 
		anAction asMenuRegistrationIn: aBuilder ].
	aBuilder