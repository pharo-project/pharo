accessing
configurationBlockForProcessorsCreatedBy: aMethod
	
	^ configurationBlocks at: aMethod printString ifAbsent: [ self nullConfigurationBlock ]