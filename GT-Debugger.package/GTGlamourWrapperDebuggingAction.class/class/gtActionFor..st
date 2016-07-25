registration
gtActionFor: aDebugger
	<gtContextCodeDebuggingAction>
	|index|
	
	index := 0.
	^ aDebugger codePresentation defaultSelectionActions collect: [ :each | 
			self new 
				glamourAction: each;
				order: (index := index + 10) ]