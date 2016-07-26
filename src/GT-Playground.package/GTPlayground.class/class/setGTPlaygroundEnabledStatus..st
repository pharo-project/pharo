settings
setGTPlaygroundEnabledStatus: aBoolean
	| oldStatus |
	
	oldStatus := self isGTPlaygroundEnabled.
	GTPlaygroundEnabledStatus := aBoolean.
	oldStatus ~= GTPlaygroundEnabledStatus ifTrue: [ 
		GTPlaygroundEnabledStatus 
			ifTrue: [ self register ]
			ifFalse: [ Workspace registerToolsOn: Smalltalk tools ] ]
	
	