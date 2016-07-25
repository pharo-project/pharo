package filter
ensureSafePackagePattern: aPattern
	^ [ aPattern asRegexIgnoringCase ]
		on: RegexSyntaxError
		do: [ :ex | self fixPackagePattern: aPattern ex: ex ]