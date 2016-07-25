opening-closing
closeGroup
	(self current notNil
			and: [self current isComposite
				and: [self current opened]])
		ifTrue: [self current closeGroup]
		ifFalse: [self close]