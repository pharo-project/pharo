testing
uses: aNode 
	^ (statements anySatisfy: [ :each | each == aNode ]) or: [ self isUsed ]