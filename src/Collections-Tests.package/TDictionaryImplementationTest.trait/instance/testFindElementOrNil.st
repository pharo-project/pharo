tests - implementation
testFindElementOrNil
	"Set>>findElementOrNil: takes an associaiton (when called on a dictionary) an integer, the index of the first
	position at is either equal to the assocation or which is nil"
	| assoc indexForG |
	assoc := #g -> 100.
	self assert: (self nonEmptyDict findElementOrNil: assoc key) = (self nonEmptyDict array indexOf: nil).
	
	indexForG := (#g hash \\ self emptyDict array size) + 1.
	self assert: (self emptyDict findElementOrNil: assoc key) = ((self emptyDict array indexOf: nil) max: indexForG).