tests
testWhenListChangedOldContents
	"access to old and new contents in whenListChanged."

	| list oldL newL ok |
	list := ListModel new.
	oldL := #(1 2).
	newL := #(1 2 3).
	list items: oldL.
	list
		whenListChanged: [ :aList :anOldList | 
			self assert: oldL = anOldList.
			self assert: newL = aList ].
	list listItems.	"Call listItems to set the cache"
	list items: #(1 2 3)