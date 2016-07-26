initialize-release
renameMethod: aSelector in: aClass to: newSel permutation: aMap 
	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
	permutation := aMap