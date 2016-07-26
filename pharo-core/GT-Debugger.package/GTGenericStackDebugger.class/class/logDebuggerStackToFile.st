settings api
logDebuggerStackToFile
	^ LogDebuggerStackToFile ifNil: [LogDebuggerStackToFile := true]