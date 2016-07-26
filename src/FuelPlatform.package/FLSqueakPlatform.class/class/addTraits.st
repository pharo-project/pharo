private-hacks
addTraits
	(Smalltalk classOrTraitNamed: #TClass) ifNil: [
		Trait 
			named: #TClass
			uses: {}
			category: self hacksCategoryName ].
	(Smalltalk classOrTraitNamed: #TClassDescription) ifNil: [
		Trait 
			named: #TClassDescription
			uses: {}
			category: self hacksCategoryName ].
	(Smalltalk classOrTraitNamed: #TBehavior) ifNil: [
		Trait 
			named: #TBehavior
			uses: {}
			category: self hacksCategoryName ].
	(Smalltalk classOrTraitNamed: #TPureBehavior) ifNil: [
		Trait 
			named: #TPureBehavior
			uses: {}
			category: self hacksCategoryName ].
	(Smalltalk classOrTraitNamed: #TApplyingOnClassSide) ifNil: [
		Trait 
			named: #TApplyingOnClassSide
			uses: {}
			category: self hacksCategoryName ]