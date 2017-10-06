creating traits
buildTrait: aTraitDefinition
	| newTrait traitComposition |
	traitComposition := aTraitDefinition traitComposition asLiteralIn: environment.
	
	newTrait := Trait
		named: aTraitDefinition traitName
		uses: traitComposition
		category: aTraitDefinition category
		env: environment.

	newTrait classTrait traitComposition: (aTraitDefinition classTraitComposition asLiteralIn: environment).

	^ newTrait