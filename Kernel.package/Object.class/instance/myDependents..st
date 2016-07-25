dependencies
myDependents: aCollectionOrNil
	"Private. Set (or remove) the receiver's dependents list."

	aCollectionOrNil
		ifNil: [DependentsFields removeKey: self ifAbsent: []]
		ifNotNil: [DependentsFields at: self put: aCollectionOrNil]