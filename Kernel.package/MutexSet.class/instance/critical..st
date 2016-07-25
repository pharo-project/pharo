mutual exclusion
critical: aBlock
	"Evaluate aBlock aquiring all mutexes"
	^self pvtCritical: aBlock startingAt: 1