private
knownLocales
	"KnownLocales := nil"
	^KnownLocales ifNil: [KnownLocales := self initKnownLocales]