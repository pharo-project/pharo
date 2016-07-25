private
privatePoolDictionaryNames
	(self isDefined and: [poolDictionaryNames isNil]) 
		ifTrue: 
			[self poolDictionaryNames: (self realClass sharedPools 
						collect: [:each | self realClass environment keyAtValue: each])].
	^poolDictionaryNames