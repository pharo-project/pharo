Extension { #name : 'NonInteractiveErrorHandler' }

{ #category : '*Morphic-Core' }
NonInteractiveErrorHandler >> onPrimitiveError: aString [
	" log error and quit "

	^ self quitFrom: thisContext sender withMessage: aString
]
