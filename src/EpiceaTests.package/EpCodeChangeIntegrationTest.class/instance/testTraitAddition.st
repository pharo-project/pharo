tests
testTraitAddition

	| trait category events event |
	trait := classFactory newTrait.
	category := trait category.
	
	events := self allLogEventsWith: EpTraitAddition.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event behaviorAffected category equals: category.