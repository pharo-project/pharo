accessing
currentVersionInfoWithMessage: aMessageString in: aRepository
	^ (self needsSaving or: [ancestry ancestors isEmpty])
		ifTrue: [ (self newVersionWithMessage: aMessageString in: aRepository) info ]
		ifFalse: [ancestry ancestors first]