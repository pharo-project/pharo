accessing pattern
ensureSafePattern: aPattern
	" do not throw an error if the pattern is bad - important in case of auto-accepting"
	^ [ aPattern asRegexIgnoringCase ]
		on: RegexSyntaxError
		do: [ :ex | ]