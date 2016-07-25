accessing
packageTreeNautilusUI: anObject
	packageTreeNautilusUI := anObject.
	"have to rebuild the roots, because the Nautilus UI just changed"
	self buildRoots