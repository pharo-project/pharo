copying
postCopy
	| penultimateLiteral |
	(penultimateLiteral := self penultimateLiteral) isMethodProperties ifTrue:
		[self penultimateLiteral: (penultimateLiteral copy
									setMethod: self;
									yourself).
		 self penultimateLiteral pragmas do:
			[:p| p setMethod: self]]
