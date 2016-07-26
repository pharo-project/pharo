private - ISO 3166
initISO3LanguageTable
	"self  initISO3LanguageTable"
	
	^ self readISOLanguagesFrom: self isoLanguages readStream.
	