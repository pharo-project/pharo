Extension { #name : #RunArray }

{ #category : #'*Deprecated90' }
RunArray >> detectSum: aBlock [
	"Only loop on runs"
	| sum |
	self deprecated: 'Use #sum: instead'.
	sum := 0.
	self runsAndValuesDo: [:run :value | 
		sum := (aBlock value: value) * run + sum].
	^ sum
]
