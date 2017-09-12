initialization
fromPackage: aPackage
	| classDefinedSorted traitsSorted allClasses|
	packageName := aPackage name.

	traitsSorted := (aPackage definedClasses select: #isTrait) sorted: [ :a :b | a traitComposition size < b traitComposition size ].
	traits := traitsSorted collect: [ :e | HETrait for: e ] as: OrderedCollection.
	
	allClasses := (aPackage definedClasses reject: #isTrait).
	classDefinedSorted := (aPackage definedClasses reject: #isTrait) sorted: [ :a :b | (self numberOfRequiredClassesOf: a in: allClasses) < (self numberOfRequiredClassesOf: b in: allClasses) ].
	classDefinedSorted do: [ :e | self addClass: (HEClass for: e) ].

	aPackage extensionMethods do: [ :e | extensionMethods add: (HEMethod for: e) ]