accessing-pragmas & properties
hasProperty: aKey
	self propertyValueAt: aKey ifAbsent: [ ^false ].
	^true.