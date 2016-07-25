versions
lastVersion

	| coll |
	coll := (self map values asArray sort: [:a :b | a <= b ]) asOrderedCollection.
	coll isEmpty ifTrue: [ ^nil ].
	^coll last