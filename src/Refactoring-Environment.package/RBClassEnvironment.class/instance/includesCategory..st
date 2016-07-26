testing
includesCategory: aCategory
	^ (super includesCategory: aCategory) and: [
		(environment classNamesFor: aCategory)
			inject: false
			into: [ :bool :each |
				bool or: [
					| class |
					class := self systemDictionary at: each ifAbsent: [ nil ].
					class notNil 
						and: [ (self includesClass: class)
						or: [ self includesClass: class class ]]]]]