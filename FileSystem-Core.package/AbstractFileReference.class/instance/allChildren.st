enumerating
allChildren
	"Return all the files and folders recursively nested in the receiver"
	
	^ CollectVisitor breadthFirst: self resolve collect: [:entry | entry reference]