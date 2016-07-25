accessing
category
	^ category ifNil: [(Smalltalk classOrTraitNamed: self baseTrait)
							ifNotNil: [:bTrait | bTrait category]
							ifNil: [self error: 'Can''t detect the category']] 