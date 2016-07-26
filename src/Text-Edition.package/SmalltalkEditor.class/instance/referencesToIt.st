menu messages
referencesToIt
	"Open a references browser on the selected symbol"

	self selectLine.
	
	self selectedSymbol 
		ifNil: [ morph flash ]
		ifNotNil: [ :symbol | self referencesTo: symbol ]
