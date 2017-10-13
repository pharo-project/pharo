initialization
fromPackage: aPackage
	| allClasses|
	packageName := aPackage name.

	traits := (aPackage definedClasses select: #isTrait) collect: [ :e | HETrait for: e ] as: OrderedCollection.	
	allClasses := (aPackage definedClasses reject: #isTrait).
	(aPackage definedClasses reject: #isTrait) do: [ :e | self addClass: (HEClass for: e) ].

	aPackage extensionMethods do: [ :e | extensionMethods add: (HEMethod for: e) ]