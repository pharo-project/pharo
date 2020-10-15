Extension { #name : #StInspectorSlotNode }

{ #category : #'*NewTools-Debugger' }
StInspectorSlotNode >> debuggerColor [
	^(SHRBTextStyler new attributesFor: #instVar) first color
]
