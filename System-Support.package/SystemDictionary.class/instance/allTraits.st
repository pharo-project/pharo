classes and traits
allTraits
	"Return all traits defined in the Smalltalk SystemDictionary"

	^ self traitNames collect: [:each | self at: each]