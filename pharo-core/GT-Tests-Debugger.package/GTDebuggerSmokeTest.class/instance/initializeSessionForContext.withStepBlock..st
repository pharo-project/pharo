initialization
initializeSessionForContext: aContext withStepBlock: aBlock
	| process |

	process := Process 
		forContext: aContext
		priority: Processor userInterruptPriority.
	
	aBlock value: process.
	session := self debuggerToTest spanNewSessionLabeled:  'test session' forContext: process suspendedContext fromProcess: process