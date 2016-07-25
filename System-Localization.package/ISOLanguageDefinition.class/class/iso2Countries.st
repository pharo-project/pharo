private - ISO 2
iso2Countries
	"self iso2Countries"
	"ISO2Countries := nil. ISO3Countries := nil"

	ISO2Countries ifNil: [self initISOCountries].
	^ISO2Countries