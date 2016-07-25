private - ISO 3
iso3LanguageTable
	"self iso3LanguageTable"

	^ISO3Table ifNil: [ISO3Table := self initISO3LanguageTable]