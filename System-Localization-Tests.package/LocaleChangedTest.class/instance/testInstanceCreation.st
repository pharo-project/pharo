tests
testInstanceCreation
	| instance |
	instance := LocaleChanged from: self englishLocale to: self germanLocale.
	
	self 
		assert: instance oldLocale = self englishLocale;
		assert: instance newLocale = self germanLocale 
	