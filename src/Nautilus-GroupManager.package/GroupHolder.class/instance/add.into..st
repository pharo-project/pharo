adding
add: aCollection into: aGroup

	aGroup
		ifAllowed: [ aGroup addAll: aCollection ]
		ifNot: [ self openReadOnlyError ]