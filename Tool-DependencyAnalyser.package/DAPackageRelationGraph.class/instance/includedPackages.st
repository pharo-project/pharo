accessing
includedPackages
	"Answer the currently analyzed set of packages."

	^ self packages select: [ :each | each isIncluded ]