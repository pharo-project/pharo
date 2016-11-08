tests
testFlatCollect
	| bag |
	bag := Bag new.
	bag add: { 1. 2. 3 }.
	bag add: { 4. 5. 6 }.

	self assert: (bag flatCollect: [ :x | x ]) equals: #(1 2 3 4 5 6) asBag.
	self assert: (bag flatCollect: [ :x | x ]) class == Bag.
	self assert: (#() asBag flatCollect: [:x | 1 to: 4 ]) isEmpty