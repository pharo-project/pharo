api-configuration
modifySymbolicVersionMethodFor: versionSymbol

	^self modifySymbolicVersionMethodFor: versionSymbol symbolicVersionSpecsDo: [:symbolicVersionSpec | true ].
