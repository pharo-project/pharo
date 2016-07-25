private
receiver
	| index rec |
	index := self item findString: self selector.
	rec := self item copyFrom: 1 to: index - 1.
	^ Smalltalk compiler evaluate: rec