instance creation
forString: aString ignoreCase: aBoolean
	"Create and answer a matcher that will match the regular expression
	`aString'."

	^self for: (RxParser new parse: aString) ignoreCase: aBoolean