accessing
reSort
	"Resort the entire heap"
	self isEmpty ifTrue:[^self].
	tally // 2 to: 1 by: -1 do:[:i| self downHeap: i].