accessing
currentVersionInfo
	^ (self needsSaving or: [ancestry ancestors isEmpty])
		ifTrue: [ (self newVersionIn: self repositoryGroup) 
						ifNotNil: [ :version | version info ] ]
		ifFalse: [ancestry ancestors first]