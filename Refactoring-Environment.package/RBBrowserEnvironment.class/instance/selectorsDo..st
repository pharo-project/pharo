accessing
selectorsDo: aBlock
	self allClassesDo: [ :each | 
		self selectorsForClass: each do: aBlock ].