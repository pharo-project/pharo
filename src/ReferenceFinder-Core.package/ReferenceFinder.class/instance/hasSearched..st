private
hasSearched: newObject 
	^backlinks anySatisfy: [:each | each includesKey: newObject]