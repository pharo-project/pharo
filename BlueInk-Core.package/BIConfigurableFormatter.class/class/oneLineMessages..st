accessing
oneLineMessages: aString
	DefaultPrettyPrintContext oneLineMessages: (self compiler evaluate: aString).
	self announceASettingChange