housekeeping
fixObsoleteSharedPools
	self allClasses
		select: [ :class |
			class sharedPools isEmptyOrNil not and: [ 
				class sharedPools contains: #isObsolete ] ]
		thenDo: [ :class |
			| obsolete |
			obsolete := class sharedPools select: #isObsolete.
			class sharedPools removeAll: obsolete ].
		
	"#sharedPools is a lazy initializer and most classes don't have a shared pool.
	Resetting the shared pools to nil should therefore save a bit of space."
	self allClasses
		select: [ :class | class sharedPools isEmpty ]
		thenDo: [ :class | class sharedPools: nil ]