settings api
setGTDebuggerEnabledStatus: aBoolean
	| oldStatus |
	
	oldStatus := self isGTDebuggerEnabled.
	GTDebuggerEnabledStatus := aBoolean.
	oldStatus ~= GTDebuggerEnabledStatus ifTrue: [ 
		GTDebuggerEnabledStatus 
			ifTrue: [ self register ]
			ifFalse: [ SpecDebugger registerToolsOn: Smalltalk tools ] ]
	
	