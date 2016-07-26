api
removeRoot: aKomitPackage
	| roots highlight |
	
	highlight := tree highlightedItem.
	
	roots := tree roots.
	roots
		remove: (KomitPackageNode new
			content: aKomitPackage;
			yourself).
		
	tree roots: roots.
	highlight takeHighlight