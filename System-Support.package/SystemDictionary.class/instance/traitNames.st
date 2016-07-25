class and trait names
traitNames
	"Answer a SortedCollection of all traits (not including class-traits) names."
	
	^self nonClassNames select: [ :name |
			self
				at: name
				ifPresent: [ :global | (global isTrait) and: [global isObsolete not]]
				ifAbsent: [ false ] ]