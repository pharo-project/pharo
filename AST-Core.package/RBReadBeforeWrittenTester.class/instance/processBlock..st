private
processBlock: aNode 
	| newScope |
	self createScope.
	self executeTree: aNode body.
	newScope := self removeScope.
	newScope keysAndValuesDo: 
			[:key :value | 
			(value == true and: [(self currentScope at: key) isNil]) 
				ifTrue: [self currentScope at: key put: value]]