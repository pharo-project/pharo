enumerating
detectSum: aBlock
	"Only loop on runs"
	| sum |
	sum := 0.
	self runsAndValuesDo: [:run :value | 
		sum := (aBlock value: value) * run + sum].
	^ sum