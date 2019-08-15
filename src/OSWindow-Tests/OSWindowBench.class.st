"
Benchmark for window processing
"
Class {
	#name : #OSWindowBench,
	#superclass : #Object,
	#category : #'OSWindow-Tests-Benchmarks'
}

{ #category : #benchmarking }
OSWindowBench >> benchEventProcessing [
	<script: 'self new benchEventProcessing'>
	
	| window sema |
	
	window := self currentWorld osWindow.
	sema := Semaphore new.
	[ 
		"TimeProfiler spyAllOn:[ sema wait ]"		
		AndreasSystemProfiler spyOn: [ sema wait ].
	] forkAt: Processor userInterruptPriority.
	
	[ 20 timesRepeat: [  
			1 to: 500 do: [ :i | 
				| event |
				event := OSMouseMoveEvent for: window.
				event 
					position: i @ 100;
					delta: 1 @ 0.
				event deliver ].
			(Delay forMilliseconds: 20) wait ].
		sema signal
	] forkAt: 60.

	

]
