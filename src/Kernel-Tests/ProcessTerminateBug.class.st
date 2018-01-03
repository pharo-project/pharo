"
SUnit tests for Process termination
"
Class {
	#name : #ProcessTerminateBug,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Processes'
}

{ #category : #tests }
ProcessTerminateBug >> testSchedulerTermination [	
	| process sema gotHere sema2 |
	gotHere := false.
	sema := Semaphore new.
	sema2 := Semaphore new.
	process := [
		sema signal.
		sema2 wait.
		"will be suspended here"
		gotHere := true. "e.g., we must *never* get here" ] forkAt: Processor activeProcess priority.
	sema wait. "until process gets scheduled"
 	process terminate.
	sema2 signal.
	Processor yield. "will give process a chance to continue and
horribly screw up"
	self assert: gotHere not.

]

{ #category : #tests }
ProcessTerminateBug >> testTerminationDuringUnwind [
	"An illustration of the issue of process termination during unwind.
	This uses a well-behaved unwind block that we should allow to complete
	if at all possible."		
	| unwindStarted unwindFinished p |
	unwindStarted := unwindFinished := false.
	p := [[] ensure:
			[ unwindStarted := true.
			Processor yield.
			unwindFinished := true. ]] fork.
	self deny: unwindStarted.
	Processor yield.
	self assert: unwindStarted.
	self deny: unwindFinished.
	p terminate.
	self assert: unwindFinished.
]

{ #category : #tests }
ProcessTerminateBug >> testUnwindFromActiveProcess [		
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert: (sema isSignaled).
	process := [ sema critical: 
						[ self deny: sema isSignaled.
						Processor activeProcess terminate.]] forkAt: Processor userInterruptPriority.
	self assert: sema isSignaled.
]

{ #category : #tests }
ProcessTerminateBug >> testUnwindFromForeignProcess [

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
]
