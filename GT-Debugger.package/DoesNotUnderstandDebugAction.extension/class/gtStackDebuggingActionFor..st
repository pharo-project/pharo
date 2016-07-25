*GT-Debugger
gtStackDebuggingActionFor: aDebugger
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousAdd;
			order: 4