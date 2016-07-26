protocol
hash
	| hash |
	hash := selectedPackage hash.
	{ selectedCategory . selectedSelector . selectedClass . selectedGroup } do: [ :iv |
		hash := hash bitXor: iv hash ].
	^ hash