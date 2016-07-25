parsing stamp
parseAuthorAliasFrom: aString
	"Allows other applications  to treat a different empty alias by overriding this method"
	
	^self basicParseAuthorAliasFrom: aString