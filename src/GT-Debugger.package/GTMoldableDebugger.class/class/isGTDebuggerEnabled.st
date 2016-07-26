settings api
isGTDebuggerEnabled

	^ GTDebuggerEnabledStatus ifNil: [ GTDebuggerEnabledStatus := Smalltalk tools debugger = self ]