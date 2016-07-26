private
processIfTrueIfFalse: aNode 
	| trueScope falseScope |
	self createScope.
	self executeTree: aNode arguments first body.
	trueScope := self removeScope.
	self createScope.
	self executeTree: aNode arguments last body.
	falseScope := self removeScope.
	self currentScope keysAndValuesDo: 
			[:key :value | 
			value isNil 
				ifTrue: 
					[(trueScope at: key) == (falseScope at: key) 
						ifTrue: [self currentScope at: key put: (trueScope at: key)]
						ifFalse: 
							[((trueScope at: key) == true or: [(falseScope at: key) == true]) 
								ifTrue: [self currentScope at: key put: true]]]]