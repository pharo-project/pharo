testing
includesCategory: aCategory 
	^(super includesCategory: aCategory) and: 
			[(self classNamesFor: aCategory) anySatisfy: 
					[:className | 
					(classSelectors includesKey: className) 
						or: [metaClassSelectors includesKey: className]]]