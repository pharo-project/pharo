parsing
docCommentRangesIn: aText
	^ '(([^[:cntrl:]]*)>>>\3?([^[:cntrl:]]*))' asRegex matchingRangesIn: aText