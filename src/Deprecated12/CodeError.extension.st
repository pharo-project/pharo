Extension { #name : 'CodeError' }

{ #category : '*Deprecated12' }
CodeError >> errorCode [

	self
		deprecated: 'errorCode is so misleading'
		transformWith: '`@receiver errorCode' -> '`@receiver sourceCode'.

	^ self sourceCode
]

{ #category : '*Deprecated12' }
CodeError >> errorMessage [

	self
		deprecated: 'As with exceptions, prefer messageText'
		transformWith: '`@receiver errorMessage' -> '`@receiver messageText'.

	^ messageText
]
