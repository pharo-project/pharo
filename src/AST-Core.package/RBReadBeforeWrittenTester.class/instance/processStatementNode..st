private
processStatementNode: aNode 
	| temps |
	(checkNewTemps not or: [aNode temporaries isEmpty]) 
		ifTrue: 
			[aNode statements do: [:each | self executeTree: each].
			^self].
	self createScope.
	temps := aNode temporaries collect: [:each | each name].
	self initializeVars: temps.
	aNode statements do: [:each | self executeTree: each].
	self removeScope keysAndValuesDo: 
			[:key :value | 
			(temps includes: key) 
				ifTrue: [value == true ifTrue: [read add: key]]
				ifFalse: 
					[(self currentScope at: key) isNil 
						ifTrue: [self currentScope at: key put: value]]]