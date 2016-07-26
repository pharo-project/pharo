asserting
assertNamesOf: versionInfoCollection are: nameArray
	| names |
	names := versionInfoCollection collect: [:ea | ea name].
	
	self assert: names asArray = nameArray