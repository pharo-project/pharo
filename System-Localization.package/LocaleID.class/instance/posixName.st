printing
posixName
	"(LocaleID isoString: 'es-MX') posixName"
	"(LocaleID isoString: 'es') posixName"
	"language[_territory]"
	^ self isoCountry
		ifNil: [self isoLanguage]
		ifNotNil: [self isoLanguage , '_' , self isoCountry]