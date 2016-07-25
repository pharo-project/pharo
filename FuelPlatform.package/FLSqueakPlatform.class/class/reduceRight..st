private-extensions-sequenceablecollection
reduceRight: aBlock
	<flExtensionOf: #SequenceableCollection>
	"Fold the result of the receiver from right to left into aBlock. The argument aBlock must take two or more arguments."

	"#(1 2 3) reduceRight: [ :a :b | a - b ].
		--> (1 - (2 - 3)) = 2
	#(1 + 3 - 5) reduceRight: [ :a :op :b | a perform: op with: b ].
		--> (1 + (3 - 5)) = -1"

	| arguments |
	self emptyCheck.
	arguments := Array new: aBlock argumentCount.
	(arguments size = 0 or: [ (self size + 1) \\ (arguments size - 1) > 0 ])
		ifTrue: [ self error: 'Collection size and block argument count do not match.' ].
	arguments at: arguments size put: self last.
	self size - arguments size + 1 to: 1 by: 1 - arguments size do: [ :index |
		arguments
			replaceFrom: 1 to: aBlock argumentCount - 1 with: self startingAt: index;
			at: arguments size put: (aBlock valueWithArguments: arguments) ].
	^ arguments last