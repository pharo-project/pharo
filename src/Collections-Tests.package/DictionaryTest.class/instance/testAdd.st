tests - adding
testAdd
	"| dict |
	dict := self emptyDict.
	dict add: #a -> 1.
	dict add: #b -> 2.
	self assert: (dict at: #a) = 1.
	self assert: (dict at: #b) = 2"
	| dictionary result |
	dictionary := self nonEmptyDict.
	result := dictionary add: self associationWithKeyNotInToAdd.
	self assert: result = self associationWithKeyNotInToAdd