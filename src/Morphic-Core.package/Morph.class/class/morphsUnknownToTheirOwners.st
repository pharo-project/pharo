misc
morphsUnknownToTheirOwners
	"Return a list of all morphs (other than HandMorphs) whose owners do not contain them in their submorph lists"

	"Morph morphsUnknownToTheirOwners"

	| problemMorphs |
	problemMorphs := OrderedCollection new.
	self allSubInstances
		do: [ :m | 
			| itsOwner |
			(m isHandMorph not and: [ (itsOwner := m owner) notNil and: [ (itsOwner submorphs includes: m) not ] ])
				ifTrue: [ problemMorphs add: m ] ].
	^ problemMorphs