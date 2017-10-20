visiting
visitPackage: aPackage
	| aHEPackage |
	aHEPackage := HEPackage new.

	aHEPackage packageName: aPackage name.
	aHEPackage traits:((aPackage definedClasses select: #isTrait) collect: [ :e | e acceptVisitor:self ] as: OrderedCollection).

	(aPackage definedClasses reject: #isTrait) do: [ :e | aHEPackage addClass: (e acceptVisitor:self) ].
	aPackage extensionMethods do: [ :e | aHEPackage extensionMethods add: (e acceptVisitor:self) ].
	
	^ aHEPackage.