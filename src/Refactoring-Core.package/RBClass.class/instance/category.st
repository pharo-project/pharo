accessing
category
	^category isNil 
		ifTrue: 
			[self isDefined
				ifTrue: [category := self realClass category]
				ifFalse: [model environment whichCategoryIncludes: self name]]
		ifFalse: [category]
