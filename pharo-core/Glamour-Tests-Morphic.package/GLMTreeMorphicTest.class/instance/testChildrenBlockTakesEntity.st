tests
testChildrenBlockTakesEntity
	| browser receivedEntity |
	browser := GLMFinder new.
	receivedEntity := nil.
	browser show: [:a | a tree
		children: [:each :entity | receivedEntity := entity. #()]].
	window := browser openOn: ($a to: $d).
	World doOneCycle.
	self repeatAssert: [receivedEntity] equals: ($a to: $d).
