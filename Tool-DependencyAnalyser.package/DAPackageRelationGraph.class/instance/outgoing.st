accessing
outgoing
	"Answer a collection of all dependencies that point out of the current package set."

	^ self includedPackages
		inject: Bag new
		into: [ :outgoing :package |
			package dependencies do: [ :each |
				(each isInternal or: [ each target isIncluded ])
					ifFalse: [ outgoing add: each ] ].
			outgoing ]