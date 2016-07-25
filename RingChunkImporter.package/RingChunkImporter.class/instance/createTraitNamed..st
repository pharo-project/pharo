private
createTraitNamed: traitName

	| trait |
	trait := RGTraitDefinition named: traitName.
	trait withMetaclass.
	package addTrait: trait.
	^ trait.