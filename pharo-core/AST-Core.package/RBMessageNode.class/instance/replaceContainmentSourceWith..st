private-replacing
replaceContainmentSourceWith: aNode 
	| originalNode needsParenthesis |
	needsParenthesis := aNode hasParentheses not and: [aNode needsParenthesis].
	originalNode := (self mappingFor: self receiver) = aNode 
				ifTrue: [self receiver]
				ifFalse: [self arguments detect: [:each | (self mappingFor: each) = aNode]].
	self
		addReplacement: (RBStringReplacement 
					replaceFrom: self start
					to: originalNode start - 1
					with: (needsParenthesis ifTrue: ['('] ifFalse: ['']));
		addReplacement: (RBStringReplacement 
					replaceFrom: originalNode stop + 1
					to: self stop
					with: (needsParenthesis ifTrue: [')'] ifFalse: ['']))