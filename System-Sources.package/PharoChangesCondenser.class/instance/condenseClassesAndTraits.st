private
condenseClassesAndTraits 
	Smalltalk allClassesAndTraitsDo: [ :classOrTrait | 
		self condenseClassOrTrait: classOrTrait	]
	