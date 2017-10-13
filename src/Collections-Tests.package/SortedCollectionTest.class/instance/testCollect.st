tests - basic
testCollect
	"self run: #testCollect"
	
	|result aSortedCollection|
	aSortedCollection := SortedCollection new.
	result := OrderedCollection new.
	result add:true ; add: true ; add: true ;add: false ; add: false.
	aSortedCollection := (1 to: 5) asSortedCollection.
	self assert: (result = (aSortedCollection collect: [:each | each < 4])).
	