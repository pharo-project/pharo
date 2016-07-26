tests - enumerating
testFlattened

	self assert: self simpleCollection flattened equals: #(1 8 3).
	self assert: self collectionOfCollectionsOfInts flattened equals: #( 1 2 3 4 5 6 ).
	self assert: self collectionWithCharacters flattened equals: #($a $x $d $c $m).
	self assert: self collectionOfCollectionsOfStrings flattened equals: #('foo' 'bar' 'zorg').