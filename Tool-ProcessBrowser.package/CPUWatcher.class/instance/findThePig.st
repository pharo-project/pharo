porcine capture
findThePig
	"tally has been updated. Look at it to see if there is a bad process.
	This runs at a very high priority, so make it fast"

	| countAndProcess process |
	countAndProcess := tally sortedCounts first.
	countAndProcess key / tally size > self threshold
		ifFalse: [ ^ self ].
	process := countAndProcess value.
	process == Processor backgroundProcess
		ifTrue: [ ^ self ].	"idle process? OK"
	self catchThePig: process