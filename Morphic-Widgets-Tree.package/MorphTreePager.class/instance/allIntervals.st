accessing
allIntervals
	| intervals |
	intervals := OrderedCollection new.
	1 to: self lastPage do: [:p | | start | intervals add: ((start := (((p - 1) * self pageSize) + 1)) -> (start + pageSize - 1))].
	intervals last value: self nodeList size.
	^ intervals