enumerating
allFiles
	"Return all the files (not directories) recursively nested in the receiver."
	^ (SelectVisitor breadthFirst: self resolve select: [:entry | entry isFile ])
		collect: [ :each| each reference ]