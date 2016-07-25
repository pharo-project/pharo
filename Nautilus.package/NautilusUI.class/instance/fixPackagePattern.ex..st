package filter
fixPackagePattern: aPattern ex: anException
	| fixedPattern |
	fixedPattern := aPattern copyReplaceAll: '*' with: '.*' .

	^  [ fixedPattern asRegexIgnoringCase ]
		on: RegexSyntaxError
		do: [ :ex | '.*' asRegexIgnoringCase ]