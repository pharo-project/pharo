comparing
= aDictionary
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value.
	See issue 16760 before changing"

	self == aDictionary ifTrue: [^true].
	self species == aDictionary species ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true