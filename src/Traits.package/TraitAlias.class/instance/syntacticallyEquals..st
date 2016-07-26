comparing
syntacticallyEquals: aTraitTransformation
	"Test syntactic equivalence of this trait transformation with another. 
	The order of aliases is taken into account. For example, A @ { #x -> #y. #z -> #w } ~= A @ { #z -> #w. #x -> #y }."

	self == aTraitTransformation ifTrue: [ ^ true ].
	self species = aTraitTransformation species ifFalse: [ ^ false ].
	^ self aliases = aTraitTransformation aliases