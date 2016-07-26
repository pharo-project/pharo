accessing
traditionalBinaryPrecedenceArray: aString
	DefaultPrettyPrintContext traditionalBinaryPrecedenceArray: (self compiler evaluate: aString).
	self announceASettingChange