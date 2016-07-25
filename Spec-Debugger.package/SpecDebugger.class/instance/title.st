api
title

	^ ((self session isContextPostMortem: self interruptedContext)  
			ifTrue: [ 'PostMortem: ' ]
			ifFalse: [ '' ]), (self label ifNil: [ '' ])