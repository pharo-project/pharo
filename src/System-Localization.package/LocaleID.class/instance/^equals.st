comparing
= anotherObject
	self class == anotherObject class
		ifFalse: [^false].
	^self isoLanguage = anotherObject isoLanguage
		and: [self isoCountry = anotherObject isoCountry]