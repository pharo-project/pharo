initialization
initializeWithPackages: aCollection
	self initialize.
	packages := Dictionary new.
	aCollection do: [ :each | self addPackage: each ].
	classToPackage := IdentityDictionary new