expression types
primaryExpression 
	hereType == #word 
		ifTrue: 
			[parseNode := self variable.
			(parseNode isUndefTemp and: [self interactive])
				ifTrue: [ self warns ifTrue: [self queryUndefined]].
			parseNode nowHasRef.
			^ true].
	hereType == #leftBracket
		ifTrue: 
			[self advance.
			self blockExpression.
			^true].
	hereType == #leftBrace
		ifTrue: 
			[self braceExpression.
			^true].
	hereType == #leftParenthesis
		ifTrue: 
			[self advance.
			self expression ifFalse: [^self expected: 'expression'].
			(self match: #rightParenthesis)
				ifFalse: [^self expected: 'right parenthesis'].
			^true].
	(hereType == #string or: [hereType == #number or: [hereType == #literal]])
		ifTrue: 
			[parseNode := encoder encodeLiteral: self advance.
			^true].
	(here == #- and: [tokenType == #number and: [1 + hereEnd = mark]])
		ifTrue: 
			[self advance.
			parseNode := encoder encodeLiteral: self advance negated.
			^true].
	^false