private-replacing
replaceSourceWithMessageNode: aNode 
	| isBinaryToKeyword newSelectorParts  oldkeywordsIntervals|
	self numArgs = aNode numArgs ifFalse: [^super replaceSourceWith: aNode].
	self arguments with: aNode arguments
		do: [:old :new | (self mappingFor: old) = new ifFalse: [^super replaceSourceWith: aNode]].
	(self mappingFor: self receiver) = aNode receiver 
		ifFalse:
			[(self receiver isVariable and: [aNode receiver isVariable])
				ifFalse:
					[^super replaceSourceWith: aNode].
			self addReplacement:
				(RBStringReplacement
					replaceFrom: self receiver start
					to: self receiver stop
					with: aNode receiver name)].
	(isBinaryToKeyword := self isBinary and: [aNode isKeyword]) 
		ifTrue: 
			[(self hasParentheses not and: [self parent precedence <= aNode precedence]) 
				ifTrue: 
					[self
						addReplacement: (RBStringReplacement 
									replaceFrom: self start
									to: self start - 1
									with: '(');
						addReplacement: (RBStringReplacement 
									replaceFrom: self stop + 1
									to: self stop
									with: ')')]].
							
	newSelectorParts := aNode selectorParts.
	oldkeywordsIntervals := self keywordsIntervals.
	
	self selectorParts keysAndValuesDo: [:index :oldPart| 
			(newSelectorParts at: index) ~= oldPart
				ifTrue: 
					[self addReplacement: (RBStringReplacement 
								replaceFrom: (oldkeywordsIntervals at: index) first
								to: (oldkeywordsIntervals at: index) last
								with: ((isBinaryToKeyword 
										and: [(self source at: (oldkeywordsIntervals at: index) first - 1) isSeparator not]) 
											ifTrue: [' ' , newSelectorParts at: index]
											ifFalse: [newSelectorParts at: index]))]].