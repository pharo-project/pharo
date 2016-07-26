misc
informNoDebugger
	(self includes: #uiManager) 
		ifTrue: [
			self uiManager inform: 'Cannot open debugger' ]
		ifFalse: [
			"If we throw an exception here, this can lead to recursion.
			Any idea what else we can do here?"
			Processor activeProcess terminate ]