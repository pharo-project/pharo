Extension { #name : #StInspectorSelfNode }

{ #category : #'*NewTools-Debugger' }
StInspectorSelfNode >> debuggerColor [
	^(SHRBTextStyler new attributesFor: #self) first color 
]
