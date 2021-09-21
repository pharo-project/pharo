Class {
	#name : #ProcessorTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Processes'
}

{ #category : #tests }
ProcessorTest >> testChangingPriorityRespectsTheProcessPreemptionSettings [
	"test whether #priority: preempts active process to allow higher priority processes run;
	#priority behavior reflects the processPreemptionYields setting - for false the active 
	process gets to the front of the queue while for true (default) it goes to the end"

	| val highPriority |
	val := nil.
	highPriority := 70.
	
	[
		[ val := false ] forkAt: highPriority + 1.
		[ val := true ] forkAt: highPriority.
		Processor activeProcess priority: highPriority.
	]	forkAt: highPriority + 2.
	
	self assert: val equals: Smalltalk vm processPreemptionYields	
]

{ #category : #tests }
ProcessorTest >> testChangingPriorityYieldsCurrentProcess [

	| val highPriority |
	val := 0.
	highPriority := 70.

	[ 
		[ val := 1 ] forkAt: highPriority + 1.
		
		Processor activeProcess priority: highPriority.
		val := 2 
	] forkAt: highPriority + 2.
	
	self assert: val equals: 2
]
