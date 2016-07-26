CPU usage tally
tallyCPUUsageFor: seconds
	"Start a high-priority process that will tally the next ready process for the given
	number of seconds. Answer a Block that will return the tally (a Bag) after the task
	is complete" 
	^self tallyCPUUsageFor: seconds every: 10
