accessing-classes
allClassesAndTraits
	" compatibility method with SystemDictionary "
	
	| allClassesAndTraits |
	allClassesAndTraits := OrderedCollection new: 4096.
	self allClassesAndTraitsDo: [ :each | 
		allClassesAndTraits add: each ].
	^ allClassesAndTraits asArray