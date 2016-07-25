accessing
orderedClasses

	| tmp |
	tmp := self definedClasses asArray sort: [:a :b | a name <= b name].
	tmp := tmp, (self extendedClasses asArray sort: [:a :b | a name <= b name]).
	^tmp collect: [:e | e theNonMetaClass ]
