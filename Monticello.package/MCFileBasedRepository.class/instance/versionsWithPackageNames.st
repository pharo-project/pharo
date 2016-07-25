accessing
versionsWithPackageNames
	| versions |
	versions := OrderedCollection new.
	self readableFileNames
		do: [ :each | 
		      self addVersionInformationExtractedFrom: each to: versions ].
	^ versions