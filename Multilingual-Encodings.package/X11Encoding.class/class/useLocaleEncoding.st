accessing
useLocaleEncoding

	self setEncodingToLocale.
	LanguageEnvironment startUp.
	^ self encoding.