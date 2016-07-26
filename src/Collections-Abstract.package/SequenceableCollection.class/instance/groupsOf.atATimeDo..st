enumerating
groupsOf: n atATimeDo: aBlock 
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end.
	Allows use of a flattened 
	array for things that naturally group into groups of n.
	If aBlock has a single argument, pass it an array of n items,
	otherwise, pass the items as separate arguments.
	See also pairsDo:"
	| passArray |
	passArray := (aBlock numArgs = 1).
	n
		to: self size
		by: n
		do: [:index | | args | 
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray ifTrue: [ aBlock value: args ]
				ifFalse: [ aBlock valueWithArguments: args ]].