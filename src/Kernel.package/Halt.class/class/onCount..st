halting
onCount: anInteger 
	"Halt on the anInteger-th time through"
	<debuggerCompleteToSender>	
	| callingContext node | 
	"if the compiler is not loaded, we fall back on the standard halt "
	self environment at: #OpalCompiler ifAbsent: [ ^self now ].
	
	callingContext := thisContext sender sender.
	node := callingContext sourceNodeExecuted.
	(node hasProperty: #haltCount) ifFalse: [
		node propertyAt: #haltCount put: 0].
	 node propertyAt: #haltCount put: (node propertyAt: #haltCount) + 1.
	((node propertyAt: #haltCount) >= anInteger) ifTrue: [ ^self signal ].