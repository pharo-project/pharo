pool variables
allSharedPools
	"Answer an ordered collection of the pools the receiver shares, including those defined  in the superclasses of the receiver."
	| aSet | 
	^self superclass == nil
		ifTrue: [self sharedPools copy]
		ifFalse: [aSet := self superclass allSharedPools.
			aSet addAll: self sharedPools.
			aSet]