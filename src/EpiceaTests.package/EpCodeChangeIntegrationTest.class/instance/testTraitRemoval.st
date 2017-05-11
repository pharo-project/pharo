tests
testTraitRemoval

	| trait category events event |
	trait := classFactory newTrait.
	category := trait category.
	trait removeFromSystem.
	
	events := self allLogEventsWith: EpTraitRemoval.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event behaviorAffected category equals: category.