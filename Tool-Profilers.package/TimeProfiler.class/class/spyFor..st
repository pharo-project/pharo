opening
spyFor: seconds
	"Run the system profiler for the specified number of seconds"
	"self spyFor: 2"
	^self spyOn: [ (Delay forSeconds: seconds) wait ].