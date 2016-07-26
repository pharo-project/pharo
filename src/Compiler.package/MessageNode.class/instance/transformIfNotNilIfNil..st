macro transformations
transformIfNotNilIfNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 1.
	((self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1)
	  and: [self checkBlock: (arguments at: 2) as: 'Nil arg' from: encoder]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := AssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := MessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments swap: 1 with: 2.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true