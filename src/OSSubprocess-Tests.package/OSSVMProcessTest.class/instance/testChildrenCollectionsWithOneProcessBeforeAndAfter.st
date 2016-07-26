tests
testChildrenCollectionsWithOneProcessBeforeAndAfter
	| command | 
	OSSVMProcess vmProcess initializeAllMyChildren. 
	command := self newCommand 
		command: '/bin/sleep';
		arguments: {'2'}.
	
	command run. 
	
	self assert: OSSVMProcess vmProcess allMyChildren size equals: 1.
	self assert: OSSVMProcess vmProcess allMyChildren first == command.
	self assert: OSSVMProcess vmProcess activeChildren size equals: 1.
	self assert: OSSVMProcess vmProcess activeChildren first == command.
	self assert: OSSVMProcess vmProcess exitedChildren isEmpty.
	self assert: OSSVMProcess vmProcess childPids size equals: 1.
	self assert: OSSVMProcess vmProcess childPids first equals: command pid.
	
	command waitForExit. 
	
	self assert: OSSVMProcess vmProcess allMyChildren size equals: 1.
	self assert: OSSVMProcess vmProcess allMyChildren first == command.
	self assert: OSSVMProcess vmProcess activeChildren size equals: 0.
	self assert: OSSVMProcess vmProcess exitedChildren size equals: 1.
	self assert: OSSVMProcess vmProcess exitedChildren first == command.
	self assert: OSSVMProcess vmProcess childPids size equals: 1.
	self assert: OSSVMProcess vmProcess childPids first equals: command pid.
			
