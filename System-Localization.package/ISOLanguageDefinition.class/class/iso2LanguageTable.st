private - ISO 2
iso2LanguageTable
	"self iso2LanguageTable"

	ISO2Table ifNotNil: [^ISO2Table].
	ISO2Table := Dictionary new: self iso3LanguageTable basicSize.
	self iso3LanguageTable do: [:entry |
		ISO2Table at: entry iso2 put: entry].
	^ISO2Table