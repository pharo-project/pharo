tests
testUnwindFromActiveProcess
	"self debug: #testUnwindFromActiveProcess"
	
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert: (sema isSignaled).
	process := [ sema critical: 
						[ self deny: sema isSignaled.
						Processor activeProcess terminate.]] forkAt: Processor userInterruptPriority.
	self assert: sema isSignaled.