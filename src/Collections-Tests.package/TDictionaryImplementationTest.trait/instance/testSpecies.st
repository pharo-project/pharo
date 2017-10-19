tests - implementation
testSpecies
	self assert: self nonEmptyDict species == self nonEmptyDict class.
	self assert: self emptyDict species == self emptyDict class