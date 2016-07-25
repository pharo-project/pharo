shared pools
allSharedPools
	"Answer a collection of the pools the receiver shares, including those defined  
	in the superclasses of the receiver."

	^ self hasSuperclass
		ifFalse: [ sharedPools ]
		ifTrue: [ self superclass allSharedPools , sharedPools ]