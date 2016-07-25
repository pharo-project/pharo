private
checkLimitForKey: aKey
	"Signal an exception when the limit, if present, is exceeded."
	
	(self limit notNil and: [ self size >= self limit and: [ (self includesKey: aKey) not ]])
		ifTrue: [ (ZnTooManyDictionaryEntries limit: self limit) signal ].
