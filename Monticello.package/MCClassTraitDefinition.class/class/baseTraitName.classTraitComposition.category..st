instance creation
baseTraitName: aString classTraitComposition: classTraitCompositionString category: aCategoryString
	^self instanceLike: (
		self new
			initializeWithBaseTraitName: aString
			classTraitComposition: classTraitCompositionString
			category: aCategoryString).