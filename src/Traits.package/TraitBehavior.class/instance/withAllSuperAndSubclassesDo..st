enumerating
withAllSuperAndSubclassesDo: aBlock
	self allSuperclassesDo: aBlock.
	aBlock value: self.
	self allSubclassesDo: aBlock