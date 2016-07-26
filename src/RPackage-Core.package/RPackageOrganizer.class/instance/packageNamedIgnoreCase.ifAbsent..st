private
packageNamedIgnoreCase: aSymbol ifAbsent: aBlock
	"In case of extensions, I can need to take a package ignoring name"
	self packagesDo: [  :each | 
		(each name sameAs: aSymbol) 
			ifTrue: [  ^ each  ]  ].
		
	^ aBlock value.