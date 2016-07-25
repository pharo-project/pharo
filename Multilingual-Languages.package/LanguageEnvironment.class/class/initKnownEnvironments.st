private
initKnownEnvironments
	"LanguageEnvironment initKnownEnvironments"

	| known |
	known := Dictionary new.
	self allSubclassesDo: [:subClass | 
		subClass supportedLanguages do: [:language | | env id | 
			env := subClass new.
			id := LocaleID isoString: language.
			env localeID: id.
			known at: id put: env]].
	^known