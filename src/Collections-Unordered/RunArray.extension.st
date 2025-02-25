Extension { #name : 'RunArray' }

{ #category : '*Collections-Unordered' }
RunArray >> asBag [
	| aBag |
	aBag := Bag new: values size.
	self runsAndValuesDo: [:run :value |
		aBag add: value withOccurrences: run].
	^aBag
]
