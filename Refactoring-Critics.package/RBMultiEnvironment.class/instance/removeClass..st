removing
removeClass: aClass
	environmentDictionaries keysDo: [ :key |
		(environmentDictionaries at: key) removeClass: aClass ]