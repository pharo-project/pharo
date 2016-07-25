iterating
traitsDo: aBlock

	self traits do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each theMetaClass ] ]