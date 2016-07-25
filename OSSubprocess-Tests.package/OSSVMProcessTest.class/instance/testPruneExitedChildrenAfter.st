tests
testPruneExitedChildrenAfter
	| command | 
	OSSVMProcess vmProcess initializeAllMyChildren. 
	command := self newCommand 
		command: '/bin/ls'.
	command runAndWait. 
	
	self assert: OSSVMProcess vmProcess exitedChildren size equals: 1.
	OSSVMProcess vmProcess pruneExitedChildrenAfter: 0. 
	
	self assert: OSSVMProcess vmProcess allMyChildren isEmpty.
	self assert: OSSVMProcess vmProcess activeChildren isEmpty.
	self assert: OSSVMProcess vmProcess exitedChildren isEmpty.
	self assert: OSSVMProcess vmProcess childPids isEmpty.
	
	