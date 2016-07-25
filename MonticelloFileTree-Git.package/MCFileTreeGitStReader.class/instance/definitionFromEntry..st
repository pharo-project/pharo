utilities
definitionFromEntry: aZipFileMember
	| defs reader |
	reader := MCStReader on: (self zipEntryContents: aZipFileMember).
	(defs := reader definitions) size <= 1
		ifFalse: [ self error: 'we should be writing exactly 1 definition per file' ].
	defs size = 0
		ifTrue: [ ^ nil ].
	^ defs first