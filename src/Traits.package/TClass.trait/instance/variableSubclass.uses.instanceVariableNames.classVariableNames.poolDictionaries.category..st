subclass creation - variable
variableSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory
	"Creates a new class as a subclass of an existing class (the receiver) 
	in which the subclass is to have indexable pointer variables."
	
	^ self
		subclass: aName
		uses: aTraitComposition
		with: [ 
			self
				variableSubclass: aName
				instanceVariableNames: someInstanceVariableNames
				classVariableNames: someClassVariableNames
				poolDictionaries: someSharedPoolNames
				category: aCategory ]