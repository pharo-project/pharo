tests
testNext
	| iterator currentElement |
	
	iterator := MTProjectBFSIterator on: MTProjectExampleBuilder projectA.
	currentElement := iterator next.
	self assert: (currentElement isKindOf: MTProject).
	self assert: currentElement name equals: 'A'.
	
	(1 to: 5) do: [ :i |
			currentElement := iterator next.
			self assert: (currentElement isKindOf: MTPackage).
			self assert:  currentElement name equals: 'PackageA' , i asString ].
	
	currentElement := iterator next.
	self assert: (currentElement isKindOf: MTDependantProject).
	self assert: currentElement name equals: 'LibXYZ'.
		
	currentElement := iterator next.
	self assert: (currentElement isKindOf: MTGroup).
	self assert: currentElement name equals: 'SmallGroup'.
	currentElement := iterator next.
	self assert: (currentElement isKindOf: MTGroup).
	self assert: currentElement name equals: 'All'.

	(1 to: 5) do: [ :i |
			currentElement := iterator next.
			self assert: (currentElement isKindOf: MTPackage).
			self assert:  currentElement name equals: 'PackageA' , i asString ].
	
	currentElement := iterator next.
	self assert: (currentElement isKindOf: MTGroup).
	self assert: currentElement name equals: 'SmallGroup'.
	
	currentElement := iterator next.
	self assert: (currentElement isKindOf: MTDependantProject).
	self assert: currentElement name equals: 'LibXYZ'.
	
	self assert: iterator hasNext not.
	self assert: iterator next isNil