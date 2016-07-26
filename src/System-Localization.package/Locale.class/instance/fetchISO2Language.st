private
fetchISO2Language
	"Locale current fetchISO2Language"
	| lang isoLang |
	lang := self primLanguage.
	lang ifNil: [^nil].
	lang := lang copyUpTo: 0 asCharacter.
	lang size = 2
		ifTrue: [^lang].
	isoLang := ISOLanguageDefinition iso3LanguageDefinition: lang.
	^isoLang
		ifNil: [nil]
		ifNotNil: [isoLang iso2]