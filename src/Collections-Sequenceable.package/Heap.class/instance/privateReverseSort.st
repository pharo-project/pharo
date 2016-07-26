private
privateReverseSort
	"Arrange to have the array sorted in reverse order.
	WARNING: this method breaks the heap invariants. It's up to the sender to restore them afterwards."
	| oldTally |
	oldTally := tally.
	[tally > 1] whileTrue:
		 [array swap: 1 with: tally.
		tally := tally - 1.
		 self downHeapSingle: 1].
	tally := oldTally