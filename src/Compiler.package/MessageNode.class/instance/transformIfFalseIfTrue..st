macro transformations
transformIfFalseIfTrue: encoder
	^(self checkBlock: (arguments at: 1) as: 'False arg' from: encoder)
	   and: [(self checkBlock: (arguments at: 2) as: 'True arg' from: encoder)
	   and: [selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
			arguments swap: 1 with: 2.
			arguments do: [:arg| arg noteOptimizedIn: self].
			true]]