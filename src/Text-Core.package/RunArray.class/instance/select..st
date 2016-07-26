enumerating
select: aBlock
	"Beware, the block will be evaluated only once per group of values."
	| newRuns newValues |
	newRuns := (Array new: runs size) writeStream.
	newValues := (Array new: values size) writeStream.
	self runsAndValuesDo: [:run :value |
		(aBlock value: value)
			ifTrue:
				[newRuns add: run.
				newValues add: value]].
	^(self class runs: newRuns contents values: newValues contents) coalesce