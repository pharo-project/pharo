refreshing
refreshCurrentSessionInfoModel

	| sessionStore text |
	sessionStore := self monitor sessionStore.

	text := String streamContents: [:stream |
		stream
			nextPutAll: 'Current Log: '; cr;
			nextPutAll: sessionStore writingFileReference basename;
			cr; cr;
			nextPutAll: 'Directory: '; cr;
			nextPutAll: sessionStore writingFileReference parent fullName.
		].

	currentSessionInfoModel enabled: false; text: text.