tests
testClassRemoval

	| className events |
	aClass := classFactory newClass.
	className := aClass name.
	aClass removeFromSystem.
	
	events := self allLogEventsWith: EpClassRemoval.
	self assert: events size equals: 1.
	"It shouldn't begin with AnObsolete"
	self assert: events anyOne behaviorAffectedName equals: className.
