accessing
frameworkMCVersion
	MCWorkingCopy managersForClass: self class do: [ :each | ^ each ancestors first name ]