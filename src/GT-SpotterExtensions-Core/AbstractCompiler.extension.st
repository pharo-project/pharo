Extension { #name : #AbstractCompiler }

{ #category : #'*GT-SpotterExtensions-Core' }
AbstractCompiler >> silentlyDo: aBlock [
	self silentlyDo: aBlock exceptionDo: [ " ignore " ]
]

{ #category : #'*GT-SpotterExtensions-Core' }
AbstractCompiler >> silentlyDo: aBlock1 exceptionDo: aBlock2 [
	aBlock1
		on: Error, SyntaxErrorNotification, OCSemanticWarning, OCSemanticError
		do: aBlock2
]
