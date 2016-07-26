enumerating
reduceLeft: aBlock
	"Fold the result of the receiver from left to right into aBlock. The argument aBlock must take two or more arguments."

	"#(1 2 3) reduceLeft: [ :a :b | a - b ].
		--> ((1 - 2) - 3) = -4
	#(1 + 3 - 5) reduceLeft: [ :a :op :b | a perform: op with: b ].
		--> ((1 + 3) - 5) = -1"

	| arguments |
	self emptyCheck.
	arguments := Array new: aBlock argumentCount.
	(arguments size = 0 or: [ (self size + 1) \\ (arguments size - 1) > 0 ])
		ifTrue: [ self error: 'Collection size and block argument count do not match.' ].
	arguments at: 1 put: self first.
	2 to: self size by: arguments size - 1 do: [ :index |
		arguments
			replaceFrom: 2 to: arguments size with: self startingAt: index;
			at: 1 put: (aBlock valueWithArguments: arguments) ].
	^ arguments first