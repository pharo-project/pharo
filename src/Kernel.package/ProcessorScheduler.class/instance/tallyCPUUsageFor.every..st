CPU usage tally
tallyCPUUsageFor: seconds every: msec
	"Start a high-priority process that will tally the next ready process for the given
	number of seconds. Answer a Block that will return the tally (a Bag) after the task
	is complete" 
	| tally sem delay endDelay |
	tally := IdentityBag new: 200.
	delay := Delay forMilliseconds: msec truncated.
	endDelay := Delay forSeconds: seconds.
	endDelay schedule.
	sem := Semaphore new.
	[
		[ endDelay isExpired ] whileFalse: [
			delay wait.
			tally add: Processor nextReadyProcess
		].
		sem signal.
	] forkAt: self highestPriority.

	^[ sem wait. tally ]