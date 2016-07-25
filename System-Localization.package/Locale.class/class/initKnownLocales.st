private
initKnownLocales
	| locales |
	locales := Dictionary new.

	"Init the locales for which we have translations"
	self defaultLocales do: [:id |
		locales at: id put: (self new localeID: id)].
	^locales