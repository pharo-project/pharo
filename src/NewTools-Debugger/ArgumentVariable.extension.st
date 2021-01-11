Extension { #name : #ArgumentVariable }

{ #category : #'*NewTools-Debugger' }
ArgumentVariable >> debuggerColor [
	^(SHRBTextStyler new attributesFor: #true) first color
]

{ #category : #'*NewTools-Debugger' }
ArgumentVariable >> variableTag [
	^ 'arg'
]
