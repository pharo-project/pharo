comparing
syntacticallyEquals: aTraitComposition
	"Test syntactic equivalence of this trait composition with another. 
	The order of the transformations is taken into account. For example, A + B ~= B + A."

	self == aTraitComposition ifTrue: [ ^ true ].
	self species = aTraitComposition species ifFalse: [ ^ false ].
	self transformations size = aTraitComposition transformations size ifFalse: [ ^ false ].
	
	self transformations with: aTraitComposition transformations do: [ :mine :their | 
		(mine syntacticallyEquals: their) ifFalse: [ ^ false ] ].

	^ true