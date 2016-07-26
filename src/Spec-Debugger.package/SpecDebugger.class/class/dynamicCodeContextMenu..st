menu code
dynamicCodeContextMenu: aBuilder
	<contextMenu>
	<specDebuggerCodeContextMenu>

	aBuilder model codeContextMenuDebuggingActions do: [ :anAction | 
		anAction asMenuRegistrationIn: aBuilder ]