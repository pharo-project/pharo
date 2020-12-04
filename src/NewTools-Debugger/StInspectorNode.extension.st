Extension { #name : #StInspectorNode }

{ #category : #'*NewTools-Debugger' }
StInspectorNode >> debuggerColor [
	^(SHRBTextStyler new attributesFor: #comment) first color
]
