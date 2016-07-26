traits
copyTraitCompositionFrom: oldClass to: newClass
(oldClass traitComposition syntacticallyEquals: newClass traitComposition )ifTrue:[^ self ].
	self 
		installTraitComposition: oldClass traitComposition copyTraitExpression 
		on: newClass.
	"We have to manually unsubcribe the newClass from its traits. Later in the class-building phase newClass becomeForward: oldClass and hence we can no longer distinguish the two. Which would leave `newClass trait users`, an IdetitySet, in an invalid state, as it's elements have been modified (the becomeForward:) without a proper rehash."
	newClass traitComposition traits do: [ :trait |
		trait removeUser: newClass ].
