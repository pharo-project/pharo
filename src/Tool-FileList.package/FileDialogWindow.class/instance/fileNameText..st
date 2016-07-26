accessing
fileNameText: aString
	"The typed file name has changed."

	fileNameText = aString asString ifTrue: [^self].
	fileNameText := aString asString.
	self updateFiles.
	self
		changed: #fileNameText;
		changed: #okEnabled.
	self selectFileFromPattern