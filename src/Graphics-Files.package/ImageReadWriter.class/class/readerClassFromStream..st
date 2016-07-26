image reading/writing
readerClassFromStream: aBinaryStream
	| readerClass |
	readerClass := self withAllSubclasses
		detect: [ :subclass | subclass understandsImageFormat: aBinaryStream ]
		ifNone: [ 
			aBinaryStream close.
			^ self error: 'image format not recognized' ].
	aBinaryStream reset.
	^ readerClass