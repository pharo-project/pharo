accessing-classes
classesAndTraitsDo: aBlock 
	self allClassesAndTraitsDo: [ :each |
		(self includesClass: each)
			ifTrue: [ aBlock value: each ] ]