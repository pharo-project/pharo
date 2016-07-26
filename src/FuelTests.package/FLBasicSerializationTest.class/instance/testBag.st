tests-collections
testBag
	| bag materialized |
	bag := Bag new.
	bag add: 10.
	bag add: 20.
	bag add: 30.
	bag add: 30.
	
	materialized := self resultOfSerializeAndMaterialize: bag.
	self assert:  bag ~~ materialized.
	self assert: bag = materialized.
	self assert: materialized size = 4.
	