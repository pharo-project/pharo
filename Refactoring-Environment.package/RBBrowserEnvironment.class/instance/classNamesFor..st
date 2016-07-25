accessing
classNamesFor: aCategoryName
	^ (self systemDictionary organization listAtCategoryNamed: aCategoryName) select: [ :each |
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		class notNil 
			and: [ (self includesClass: class)
			or: [ self includesClass: class class ] ] ]