enumerating
flatCollect: aBlock as: aCollectionClass
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. Equivalent to the lisp's mapcan"
	| col |
	col := aCollectionClass new: self size. 	
	self do: [ :each |
		col addAll: (aBlock value: each) ].
	^col