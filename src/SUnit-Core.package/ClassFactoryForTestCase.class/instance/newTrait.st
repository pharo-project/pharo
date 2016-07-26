creating
newTrait
	^ self
		newTraitNamed: self newTraitName
		uses: Array new
		category: self defaultCategoryPostfix