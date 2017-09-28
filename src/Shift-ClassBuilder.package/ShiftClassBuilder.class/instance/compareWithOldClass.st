changes
compareWithOldClass
	oldClass ifNil: [ ^ self ].
	changeComparers do: [ :e | e compareClass: oldClass with: self ]