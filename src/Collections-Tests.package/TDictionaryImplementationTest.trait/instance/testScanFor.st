tests - implementation
testScanFor
	"Set>>scanFor: return an integer "
	| assoc indexForG |
	assoc := #g -> 100.
	self assert: (self nonEmptyDict scanFor: assoc) = (self nonEmptyDict array indexOf: nil).

	indexForG := (#g hash \\ self emptyDict array size) + 1.
	self assert: (self emptyDict scanFor: assoc) = ((self emptyDict array indexOf: nil) max: indexForG).
