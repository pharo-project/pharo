parsing
parseDocComments: aRBCommentNode
	| ranges |
	ranges := self docCommentRangesIn: aRBCommentNode contents.
	^ ranges
		collect: [ :range | 
			| expression result exps |
			exps := '>>>' split: (aRBCommentNode contents copyFrom: range first to: range last).
			expression := PharoDocCommentExpression new
				expressionInterval: (range first to: range first + exps first size - 1);
				source: aRBCommentNode contents.
			result := PharoDocCommentExpression new
				expressionInterval: (1 + range last - exps last size to: range last);
				source: aRBCommentNode contents.
			self expression: expression result: result inComment: aRBCommentNode ]