actions
executeAction

	Clipboard clipboardText: (String streamContents: [ :s| 
		self debugger interruptedContext shortDebugStackOn: s ])