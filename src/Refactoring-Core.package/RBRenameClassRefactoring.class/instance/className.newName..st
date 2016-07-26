initialize-release
className: aName newName: aNewName 
	className := aName asSymbol.
	class := self model classNamed: className.
	newName := aNewName asSymbol