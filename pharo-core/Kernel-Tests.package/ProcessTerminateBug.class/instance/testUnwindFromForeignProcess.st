tests
testUnwindFromForeignProcess
	"self debug:#testUnwindFromForeignProcess"

	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert: sema isSignaled.
	process := [ 
	sema
		critical: [ 
			self deny: sema isSignaled.
			sema wait	"deadlock" ] ] forkAt: Processor userInterruptPriority.
	self deny: sema isSignaled.	"This is for illustration only - the BlockCannotReturn cannot 
	be handled here (it's truncated already)"
	process terminate.
	self assert: sema isSignaled