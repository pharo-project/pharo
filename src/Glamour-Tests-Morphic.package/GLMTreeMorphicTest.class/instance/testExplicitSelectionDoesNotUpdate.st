tests
testExplicitSelectionDoesNotUpdate
	| composite |
	composite := GLMCompositePresentation new
		with: [ :c | c tree 
			allowsRecursiveSearch: false;
			children: [ :each | each  to: each + 10  ] ].
	window := composite openOn: { 1 }.
	
	composite selection: Object new.

	self assert:  self tree selectedMorph equals: nil