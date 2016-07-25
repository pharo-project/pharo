private
internalGetText	

	^TextDiffBuilder
		 buildDisplayPatchFrom: ((self priorVersionOfAMethod: method) sourceCode) 
		to: (method sourceCode) 
		inClass: (method className)