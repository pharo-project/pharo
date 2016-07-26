accessing
cumulativeCounts
	"Answer with a collection of cumulative percents covered by elements so far."
	| s n |
	s := self size / 100.0. n := 0.
	^ self sortedCounts asArray collect:
		[:a | n := n + a key. (n / s roundTo: 0.1) -> a value]