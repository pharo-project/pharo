iterating
definedClassesDo: aBlock

	self definedClasses do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each theMetaClass ] ]