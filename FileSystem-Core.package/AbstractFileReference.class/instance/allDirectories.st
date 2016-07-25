enumerating
allDirectories
	"Return all the directories recursively nested in the receiver."
	^ (SelectVisitor breadthFirst: self resolve select: [:entry | entry isDirectory ])
		collect: [ :each| each reference ]