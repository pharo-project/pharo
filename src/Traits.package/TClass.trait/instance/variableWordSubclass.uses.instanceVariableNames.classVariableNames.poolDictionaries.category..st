subclass creation - variableWord
variableWordSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory
	^ self
		variableWordSubclass: aName
		uses: aTraitComposition
		instanceVariableNames: someInstanceVariableNames
		classVariableNames: someClassVariableNames
		poolDictionaries: someSharedPoolNames
		package: aCategory