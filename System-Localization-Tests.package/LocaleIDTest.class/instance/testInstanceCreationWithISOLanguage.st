tests
testInstanceCreationWithISOLanguage

	|germanLocale|
	germanLocale := LocaleID isoLanguage: 'de'.
	self assert: germanLocale isoLanguage = 'de'