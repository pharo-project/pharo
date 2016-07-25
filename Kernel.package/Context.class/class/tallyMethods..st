examples
tallyMethods: aBlock
	"This method uses the simulator to count the number of calls on each method
	invoked in evaluating aBlock. Results are given in order of decreasing counts."
	| prev tallies |
	tallies := Bag new.
	prev := aBlock.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep:
			[:current |
			current == prev ifFalse: "call or return"
				[prev sender ifNotNil: "call only"
					[tallies add: current printString].
				prev := current]].
	^ tallies sortedCounts

	"Contex tallyMethods: [3.14159 printString]"