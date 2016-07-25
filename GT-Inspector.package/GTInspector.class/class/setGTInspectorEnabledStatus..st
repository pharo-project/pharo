settings
setGTInspectorEnabledStatus: aBoolean
	| oldStatus |
	
	oldStatus := self isGTInspectorEnabled.
	GTInspectorEnabledStatus := aBoolean.
	oldStatus ~= GTInspectorEnabledStatus ifTrue: [ 
		GTInspectorEnabledStatus 
			ifTrue: [ self register ]
			ifFalse: [ EyeInspector registerToolsOn: Smalltalk tools ] ]
	
	