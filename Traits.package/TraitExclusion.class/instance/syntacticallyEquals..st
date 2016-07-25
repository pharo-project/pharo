comparing
syntacticallyEquals: aTraitTransformation
	"Test syntactic equivalence of this trait transformation with another. 
	The order of the exclusions is taken into account. For example, A - { #x. #y } ~= A - { #y. #x }."

	self == aTraitTransformation ifTrue: [ ^ true ].
	self species = aTraitTransformation species ifFalse: [ ^ false ].
	^ self exclusions = aTraitTransformation exclusions