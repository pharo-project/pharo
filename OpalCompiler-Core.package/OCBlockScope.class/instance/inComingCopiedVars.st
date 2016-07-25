accessing
inComingCopiedVars
	^ copiedVars select: [:each | outerScope copiedVars includes: each].