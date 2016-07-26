enumerating
months

	| months |
	months := OrderedCollection new: 12.
	self monthsDo: [ :m | months add: m ].
	^ months asArray.