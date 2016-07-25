building
packageSearchUpdate: aPattern
	packagePattern := (aPattern isNil or: [ aPattern trimBoth isEmpty ])
		ifTrue: [ nil ]
		ifFalse: [ [aPattern asRegexIgnoringCase] on: RegexSyntaxError do: [ nil ] ].
	self updatePackages.