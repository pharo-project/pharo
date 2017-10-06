as yet unclassified
doTestExportingPackage: aPackageNamed
	| new |
 
	self fillOldPackage: aPackageNamed.

	old writeInto: writer.
	writer flush.
	
	new := HEPackage readFrom: reader.
	
	self assert: old classes size equals: (aPackage definedClasses reject:#isTrait) size.
	self assert: old traits size equals: (aPackage definedClasses select:#isTrait) size.

	self assert: old extensionMethods size equals: (aPackage extensionMethods) size.
	
	self assert: ( old classes allSatisfy: [ :e | self in: old classes allSuperclassesAreBeforeThan: e] ).
	self assert: ( old classes allSatisfy: [ :e | self in: old classes allSharedPoolsAreBeforeThan: e] ).
	self assert: ( old traits allSatisfy: [ :e | self in: old traits allTraitsAreBeforeThan: e] ).

	old traits with: new traits do: [ :a :b | self assert: a equals:b ].
	old classes with: new classes do: [ :a :b | self assert: a equals:b ].
	old extensionMethods with: new extensionMethods do: [ :a :b | self assert: a equals:b ].

	self assert: new equals: old.