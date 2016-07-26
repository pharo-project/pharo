private
subclassOf: aClass overrides: aSelector 
	^(aClass subclasses detect: 
			[:each | 
			(each includesSelector: aSelector)
				or: [self subclassOf: each overrides: aSelector]]
		ifNone: [nil]) notNil