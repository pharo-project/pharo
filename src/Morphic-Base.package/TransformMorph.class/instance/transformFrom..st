event handling
transformFrom: uberMorph 
	"Return a transform to map coorinates of uberMorph, a morph above me in my owner chain, into the coordinates of my submorphs."

	(self == uberMorph or: [owner isNil]) ifTrue: [^transform].
	^(owner transformFrom: uberMorph) composedWithLocal: transform