transforming
parseInlineMethod
	self inlineClass isNil 
		ifTrue: 
			[self 
				refactoringFailure: ('<1p> or its superclasses don''t contain method <2s>' 
						expandMacrosWith: class
						with: self inlineSelector)].
	inlineParseTree := self inlineClass parseTreeFor: self inlineSelector.
	inlineParseTree isNil 
		ifTrue: [self refactoringFailure: 'Could not parse sources'].
	inlineParseTree lastIsReturn ifFalse: [inlineParseTree addSelfReturn]