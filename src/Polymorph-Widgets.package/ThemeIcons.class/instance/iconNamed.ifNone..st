accessing
iconNamed: aSymbol ifNone: aBlock

	self icons at: aSymbol asSymbol ifPresent: [ :icon | ^ icon ].
	"Trying the old way"
	
	((aSymbol endsWith: 'Icon') or: [ (aSymbol endsWith: 'Form') ]) ifTrue: [ 
		self icons 
			at: (aSymbol allButLast: 4) asSymbol 
			ifPresent: [ :icon | ^ icon ] 
	].

	^ aBlock value