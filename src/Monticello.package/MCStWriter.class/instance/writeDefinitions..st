writing
writeDefinitions: aCollection
	
	"the correct initialization order is unknown if some classes are missing in the image"
	
	| presentInitializers notPresentInitializers orderedClasses |
	
	initializers := Set new.

	(MCDependencySorter sortItems: aCollection)
		do: [:ea | ea accept: self]
		displayingProgress: 'Writing definitions...'.
		
	presentInitializers := initializers select: [:each | Smalltalk hasClassNamed: each key ].
	notPresentInitializers := initializers reject: [:each | Smalltalk hasClassNamed: each key ].
	
	orderedClasses := (Class superclassOrder: (presentInitializers collect: [:each | Smalltalk classOrTraitNamed: each key]))
		collect: [:each | each name ].
				
	orderedClasses do: [:className |
		self chunkContents: [ :str | str nextPutAll: (presentInitializers detect: [:each | each key = className]) value contents] ].
	
	(notPresentInitializers asSortedCollection: [:a :b | a key <= b key]) do: [:association |
		self chunkContents: [ :str | str  nextPutAll: association value contents ] ].