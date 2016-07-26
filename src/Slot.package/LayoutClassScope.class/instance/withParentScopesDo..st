enumerating
withParentScopesDo: aBlock
	aBlock value: self.
	parentScope withParentScopesDo: aBlock.