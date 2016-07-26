instance creation
forContext: aContext priority: anInteger 
	"Answer an instance of me that has suspended aContext at priority 
	anInteger."

	| newProcess |
	newProcess := self new.
	newProcess suspendedContext: aContext.
	newProcess priority: anInteger.
	Processor activeProcess installEnvIntoForked: newProcess.
	^newProcess