transforming
renameImplementors
	self implementors do: 
			[:each | 
			| parseTree |
			parseTree := each parseTreeFor: oldSelector.
			parseTree isNil 
				ifTrue: [self refactoringFailure: 'Could not parse source code.'].
			self implementorsCanBePrimitives 
				ifFalse: 
					[parseTree isPrimitive 
						ifTrue: 
							[self refactoringFailure: ('<1p>''s implementation of #<2s> is a primitive' 
										expandMacrosWith: each
										with: oldSelector)]].
			self modifyImplementorParseTree: parseTree in: each.
			(each methodFor: oldSelector) compileTree: parseTree]