CPU utilization
tallyCPUUsageFor: seconds
	"Compute CPU usage using a 10-msec sample for the given number of seconds,
	then dump the usage statistics on the Transcript. The UI is free to continue, meanwhile"
	"ProcessBrowser tallyCPUUsageFor: 10"
	^self tallyCPUUsageFor: seconds every: 10