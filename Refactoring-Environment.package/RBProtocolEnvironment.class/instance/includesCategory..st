testing
includesCategory: aCategory
	^ (super includesCategory: aCategory) and: [
		(environment classNamesFor: aCategory)
			inject: false into: [ :bool :each |
				bool or: [
					| aClass |
					aClass := self systemDictionary at: each ifAbsent: [ nil ].
					aClass == class or: [ aClass class == class ] ] ] ]