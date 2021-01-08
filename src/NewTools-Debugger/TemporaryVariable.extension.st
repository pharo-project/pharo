Extension { #name : #TemporaryVariable }

{ #category : #'*NewTools-Debugger' }
TemporaryVariable >> debuggerColor [
	^(SHRBTextStyler new attributesFor: #string) first color
]

{ #category : #'*NewTools-Debugger' }
TemporaryVariable >> variableTag [
	^ 'temp'
]
