private - ISO 3
iso3Countries
	"self iso3Countries"
	"ISO2Countries := nil. ISO3Countries := nil"

	ISO3Countries ifNil: [self initISOCountries].
	^ISO3Countries