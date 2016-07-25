accessing-classes
classesAndTraits
	
	| classesAndTraits |
	classesAndTraits := OrderedCollection new: 4096.
	self classesAndTraitsDo: [ :each | 
		classesAndTraits add: each ].
	^ classesAndTraits asArray