subclass creation - weak
weakSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: aCategory
	"Creates a new class as a subclass of an existing class (the receiver) 
	in which the subclass is to have weak indexable pointer variables."
	
	^self weakSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory