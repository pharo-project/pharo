accessing
multiLineMessages: aString
	DefaultPrettyPrintContext multiLineMessages: (self compiler evaluate: aString).
	self announceASettingChange