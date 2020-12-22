Extension { #name : #StInspectorStackTopNode }

{ #category : #'*NewTools-Debugger' }
StInspectorStackTopNode >> debuggerColor [
	^(SHRBTextStyler new attributesFor: #thisContext) first color
]
