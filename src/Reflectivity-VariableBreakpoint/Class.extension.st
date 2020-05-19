Extension { #name : #Class }

{ #category : #'*Reflectivity-VariableBreakpoint' }
Class >> link: aMetaLink toVariableNamed: vName option: accessStrategy [
	^ self link: aMetaLink toSlotNamed: vName option: accessStrategy
]

{ #category : #'*Reflectivity-VariableBreakpoint' }
Class >> newBreakpointForVariable: aVariableName [
	^ VariableBreakpoint
		newBreakpointForVariable: aVariableName
		inClass: self
]
