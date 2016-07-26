tests
testInitializeChildren

	OSSVMProcess vmProcess initializeAllMyChildren. 
	
	self assert: OSSVMProcess vmProcess allMyChildren isEmpty.
	self assert: OSSVMProcess vmProcess activeChildren isEmpty.
	self assert: OSSVMProcess vmProcess exitedChildren isEmpty.
	self assert: OSSVMProcess vmProcess childPids isEmpty.
	
	
			
