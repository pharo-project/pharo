settings
logDebuggerStackToFile
	^ LogDebuggerStackToFile ifNil: [LogDebuggerStackToFile := true]