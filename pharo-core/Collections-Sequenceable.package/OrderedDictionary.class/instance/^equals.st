comparing
= anObject
	self == anObject
		ifTrue: [^ true].

	(self species == anObject species
		and: [self size = anObject size])
		ifFalse: [^ false].

	dictionary associationsDo: [:each |
		(anObject at: each key ifAbsent: [^ false]) = each value
			ifFalse: [^ false]].
	^ true.