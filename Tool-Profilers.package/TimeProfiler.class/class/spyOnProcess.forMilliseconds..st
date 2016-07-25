opening
spyOnProcess: aProcess forMilliseconds: msecDuration 
	"Run aProcess for msecDuration milliseconds, then open a TimeProfiler on the results."

	| inst |
	inst := self new.
	inst withBlockCodePane: false.
	inst runProcess: aProcess forMilliseconds: msecDuration pollingEvery: MessageTally defaultPollPeriod.
	inst open.
	^ inst