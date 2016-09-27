tests
testSorted
	| result tmp unsorted |
	unsorted := self unsortedCollection.
	result := unsorted sorted.
	self deny: unsorted == result.
	tmp := result at: 1.
	result
		do: [ :each | 
			self assert: each >= tmp.
			tmp := each ]