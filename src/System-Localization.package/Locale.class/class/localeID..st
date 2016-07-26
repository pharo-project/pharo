accessing
localeID: id
	^self knownLocales at: id ifAbsentPut: [Locale new localeID: id]