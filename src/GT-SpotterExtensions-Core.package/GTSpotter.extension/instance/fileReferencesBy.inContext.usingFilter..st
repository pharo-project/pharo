*GT-SpotterExtensions-Core-private
fileReferencesBy: aSelector inContext: aSpotterContext usingFilter: aBlock
	| split rootDirectory query |
	split := aSpotterContext textLowercase piecesCutWhere: [ :a :b | a = FileSystem disk delimiter ].
	split size < 1
		ifTrue: [ 
			query := aSpotterContext textLowercase.
			rootDirectory := FileSystem workingDirectory ]
		ifFalse: [ 
			(split last last = FileSystem disk delimiter)
				ifTrue: [ 
					query := ''.
					rootDirectory := aSpotterContext textLowercase asFileReference ]
				ifFalse: [ 
					query := split last.
					rootDirectory := ('' join: split allButLast) asFileReference ] ].
	rootDirectory exists ifFalse: [ ^ self ].
	self flag: 'this is an ugly hack for #queryForFileReference. This iterator has to manipulate the query in order for the filter to work properly'.
	aSpotterContext cacheAt: #queryForFileReference put: query.
	(rootDirectory perform: aSelector) do: aBlock