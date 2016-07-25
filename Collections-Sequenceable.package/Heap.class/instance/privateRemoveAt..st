private
privateRemoveAt: index
	"Remove the element at the given index and make sure the sorting order is okay"
	| removed |
	removed := array at: index.
	array at: index put: (array at: tally).
	array at: tally put: nil.
	tally := tally - 1.
	index > tally ifFalse:[
		"Use #downHeapSingle: since only one element has been removed"
		self downHeapSingle: index].
	^removed