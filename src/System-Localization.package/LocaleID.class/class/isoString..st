instance creation
isoString: isoString
	"Parse the isoString (<language>-<country>) into its components and return the matching LocaleID"
	"LocaleID isoString: 'en' "
	"LocaleID isoString: 'en-us' "

	| parts language country |
	parts := isoString findTokens: #($- ).
	language := parts first.
	parts size > 1
		ifTrue: [country := parts second].
	^self isoLanguage: language isoCountry: country