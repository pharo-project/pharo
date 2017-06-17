accessing
mcRepository
	| repository tuple extractedLocation |
	
	tuple := self splitRootAndSubdirectoryFromLocation.
	extractedLocation := tuple first asFileReference asAbsolute.
	repository := IceRepository registry 
		detect: [ :each | each location asAbsolute = extractedLocation ]
		ifNone: [ 
			(IceRepositoryCreator new 
				location: tuple first;
				subdirectory: tuple second;
				createRepository)
				register ].
			
	^ repository metacelloAdapter