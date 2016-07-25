CPU utilization
dumpTallyOnTranscript: tally
	"tally is from ProcessorScheduler>>tallyCPUUsageFor:
	Dumps lines with percentage of time, hash of process, and a friendly name"

	self dumpTally: tally on: Transcript.
	Transcript flush.