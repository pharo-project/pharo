accessing
packages
	| packages |
	packages := Set new.
	self arguments 
		reject: [ :arg| arg beginsWith: '-']
		thenDo: [ :arg| self addPackagesMatching: arg to: packages ].
	^ packages