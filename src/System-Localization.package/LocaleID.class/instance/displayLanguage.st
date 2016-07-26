accessing
displayLanguage
	| language |
	language := (ISOLanguageDefinition iso2LanguageTable
				at: self isoLanguage
				ifAbsent: [^ self isoLanguage]) language.
	^ self isoCountry
		ifNil: [language]
		ifNotNil: [language , ' (' , self displayCountry , ')']