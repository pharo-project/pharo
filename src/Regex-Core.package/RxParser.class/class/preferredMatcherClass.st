preferences
preferredMatcherClass
	"The matcher to use. For now just one is available, but in
	principle this determines the matchers built implicitly,
	such as by String>>asRegex, or String>>matchesRegex:.
	This might seem a bit strange place for this preference, but
	Parser is still more or less `central' thing in the whole package."

	^RxMatcher