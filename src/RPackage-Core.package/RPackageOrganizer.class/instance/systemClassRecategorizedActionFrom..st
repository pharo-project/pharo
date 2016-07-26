system integration
systemClassRecategorizedActionFrom: ann
	| class newRPackage newRPackageTag oldRPackage newCategoryName oldCategoryName|
	
	class := ann classAffected.	

	newCategoryName := ann newCategory asSymbol.
	oldCategoryName := ann oldCategory asSymbol.
	oldRPackage := self packageMatchingExtensionName: oldCategoryName includingClass: class.
	newRPackage := (self packageMatchingExtensionName: newCategoryName) 
		ifNil: [ self registerPackage: (self packageClass named: newCategoryName) ].

	newRPackageTag := newRPackage addClassTag: newCategoryName.

	newRPackage 
		moveClass: class 
		fromPackage: oldRPackage 
		toTag: newRPackageTag