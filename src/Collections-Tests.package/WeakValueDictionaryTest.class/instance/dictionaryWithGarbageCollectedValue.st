accessing
dictionaryWithGarbageCollectedValue

	| dictionary |
	dictionary := self classToBeTested new.
	dictionary at: self keyWithGarbageCollectedValue put: Object new.
	Smalltalk garbageCollect.
	^ dictionary
