api-configuration
modifyVersionMethodForVersion: inputVersionStringOrSymbol
	^ self modifyVersionMethodForVersion: inputVersionStringOrSymbol versionSpecsDo: [ :attribute :versionSpec | true ]