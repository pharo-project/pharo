tests
testWhenListChangedOldContents
	"access to old and new contents in whenListChanged."

	| oldL newL |
	oldL := #(1 2).
	newL := #(1 2 3).
	testedInstance items: oldL.
	testedInstance
		whenListChanged: [ :aList :anOldList | 
			self assert: oldL = anOldList.
			self assert: newL = aList ].
	testedInstance listItems.	"Call listItems to set the cache"
	testedInstance items: #(1 2 3)