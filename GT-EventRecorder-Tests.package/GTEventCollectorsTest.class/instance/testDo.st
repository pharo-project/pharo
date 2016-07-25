tests
testDo
	"Ensure that collectors are iterated in the same order as they are added."
	| collector1 collector2 interatedCollectors |
	collector1 := GTEventCollector new.
	collector2 := GTEventCollector new.
	collectors add: collector2.
	collectors add: collector1.
	interatedCollectors := OrderedCollection new.
	collectors do: [ :eachCollector | interatedCollectors addLast: eachCollector ].
	self assert: interatedCollectors size equals: 2.
	self assert: interatedCollectors first == collector2.
	self assert: interatedCollectors second == collector1.
	