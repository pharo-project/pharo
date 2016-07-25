CPU utilization
tallyCPUUsageFor: seconds every: msec
	"Compute CPU usage using a msec millisecond sample for the given number of seconds,
	then dump the usage statistics on the Transcript. The UI is free to continue, meanwhile"
	"ProcessBrowser tallyCPUUsageFor: 10 every: 100"

	| promise |
	promise := Processor tallyCPUUsageFor: seconds every: msec.

	[ | tally |
		tally := promise value.
		UIManager default defer: [ self dumpTallyOnTranscript: tally ].
	] fork.