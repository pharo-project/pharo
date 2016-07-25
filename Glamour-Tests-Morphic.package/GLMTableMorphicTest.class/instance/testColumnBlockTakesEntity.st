tests
testColumnBlockTakesEntity

	| browser receivedEntity |
	browser := GLMFinder new.
	receivedEntity := nil.
	browser show: [:a | a table
		column: 'Even' evaluated: [:each :entity | receivedEntity := entity. 'x']].
	window := browser openOn: (1 to: 5).
	World doOneCycle.
	self repeatAssert: [receivedEntity] equals: (1 to: 5).
