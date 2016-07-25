iterating elements
elementsDo: aBlock
	elements valuesDo: [ :collection | collection do: [ :each | aBlock value: each ] ]