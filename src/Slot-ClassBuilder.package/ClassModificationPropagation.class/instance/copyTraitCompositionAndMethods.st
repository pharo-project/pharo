private
copyTraitCompositionAndMethods
	newClass traitComposition: self target traitComposition.
	(self target methods select: #isFromTrait)
		do: [ :m | newClass methodDict at: m selector put: m ].
	newClass traitComposition allTraits
		do: [ :t | 
			t removeUser: self target.
			t addUser: newClass ]