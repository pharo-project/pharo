CPU utilization
dumpPigStackOn: aStream andClose: aBoolean
	"Must run forked on its own process, so the monitored behavior is not affected too much" 

	| promise tally process depth stack suspendedContext |
	promise := Processor tallyCPUUsageFor: 1 every: 10.
	tally := promise value.
	"WorldState addDeferredUIMessage: [self dumpTallyOnTranscript: tally]."
	aStream nextPutAll: '====Al processes===='; cr.
	self dumpTally: tally on: aStream.
	aStream cr; nextPutAll: '====Process using most CPU===='; cr.
	process := tally sortedCounts first value.
	(100.0 * (tally occurrencesOf: process) / tally size) rounded printOn: aStream.
	aStream
		nextPutAll: ' % ';
		nextPutAll: (process browserPrintStringWith: (self nameAndRulesFor: process) first);
		cr.
	depth := 20.
	stack := process == Processor activeProcess
		ifTrue: [thisContext stackOfSize: depth]
		ifFalse: [suspendedContext := process suspendedContext.
			suspendedContext
				ifNotNil: [suspendedContext stackOfSize: depth]].
	stack 
		ifNil: [ aStream nextPutAll: 'No context'; cr]
		ifNotNil: [
			stack do: [ :c | 
				c printOn: aStream.
				aStream cr]].
	aBoolean ifTrue: [aStream close]