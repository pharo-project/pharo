private
processToInterrupt 
	"Look for best candidate to interrupt: 
		- any scheduled non-finalization process of lower priority 
		- the weak-finalization process, if scheduled 
		- the UI process 
	Never interrupt the idle process, since killing it is fatal" 

	| fallback | 
	fallback := UIManager default uiProcess. 
	Processor 
		scanSchedule: [:p | 
		 "suspendedContext sender == nil usually means that process is only scheduled but had no chance to run" 
			(p ~~ Processor backgroundProcess and: [p suspendedContext sender notNil]) ifTrue: [
				p ~~  WeakArray runningFinalizationProcess 
					ifTrue: [^p] 
					ifFalse: [fallback := p]]]
		startingAt: Processor activePriority. 
	^fallback