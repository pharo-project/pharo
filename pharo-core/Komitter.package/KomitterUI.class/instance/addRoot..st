api
addRoot: aPackage
	| aPackageNode |
	
	aPackageNode := KomitTreeBuilder buildRootForPackage: aPackage.
	
	tree roots: (tree roots
		add: aPackageNode;
		sorted)