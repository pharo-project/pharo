private
browse
	self item isSymbol	ifTrue: [ 	
		^ SystemNavigation new browseAllImplementorsOf: self item ].
	self item browse.