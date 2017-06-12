tests
testDistribution
	| results occurences generator |
	1 to: 100 do: [ :i |
		results := Bag new.
		generator := Random new seed: 1234567+i.
		2000 timesRepeat: [ results add: (100 atRandom: generator) ].
		occurences := Array new: 100 streamContents: [ :out |
			results doWithOccurrences: [ :element :occurence |
				self assert: (element between: 1 and: 100).
				out nextPut: occurence.
				self assert: (occurence between: 5 and: 40) ] ].
		self assert: occurences average equals: 20.
		self assert: occurences stdev < 10].