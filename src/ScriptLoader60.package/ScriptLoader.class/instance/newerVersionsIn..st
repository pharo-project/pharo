load primitives
newerVersionsIn: aCollection
	^aCollection reject: [:each |
		self allManagers anySatisfy: [:workingcopy |
			workingcopy ancestry ancestorString , '.mcz' = each]].