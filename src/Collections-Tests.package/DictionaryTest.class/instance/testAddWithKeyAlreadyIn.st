tests - adding
testAddWithKeyAlreadyIn
	| dictionary result association |
	dictionary := self nonEmptyDict.
	association := self associationWithKeyNotInToAdd.
	result := dictionary add: association.
	self assert: result = association.
	self assert: (dictionary at: association key) = association value