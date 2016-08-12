accessing-pragmas & properties
hasProperty: aKey
	self propertyAt: aKey ifAbsent: [ ^false ].
	^true.