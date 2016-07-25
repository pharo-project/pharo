CPU utilization
dumpTally: tally on: aStream
	"tally is from ProcessorScheduler>>tallyCPUUsageFor:
	Dumps lines with percentage of time, hash of process, and a friendly name"

	tally sortedCounts do: [ :assoc | | procName |
		procName := (self nameAndRulesFor: assoc value) first.
		(((assoc key / tally size) * 100.0) roundTo: 1) printOn: aStream.
		aStream
			nextPutAll: '%   ';
			print: assoc value identityHash; space;
			nextPutAll: procName;
			cr.
	]