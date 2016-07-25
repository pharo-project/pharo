attributes
processHasThreadIdInstVar: aBoolean
	"The threaded VM needs to know if the 4th inst var of Process
	 is threadId which it uses to control process-to-thread binding.
	 This flag persists across snapshots, stored in the image header."
	aBoolean ifTrue: [self assert: (Process instVarNames at: 4) ='threadId'].
	self parameterAt: 48 put: ((self parameterAt: 48) bitClear: 1) + (aBoolean asBit)
	
