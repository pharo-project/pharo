Extension { #name : #StInspectorThisContextNode }

{ #category : #'*NewTools-Debugger' }
StInspectorThisContextNode >> debuggerColor [
	^(SHRBTextStyler new attributesFor: #thisContext) first color
]
