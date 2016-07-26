iterating
classesDo: aBlock

	self classes do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each theMetaClass ] ]