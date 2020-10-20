Extension { #name : #Class }

{ #category : #'*Reflectivity-VariableBreakpoint' }
Class >> link: aMetaLink toVariableNamed: vName option: accessStrategy [
	^aMetaLink
		installOnVariableNamed: vName
		for: self
		option: accessStrategy
		instanceSpecific: self intanceSpecificMetaLinksAvailable
]

{ #category : #'*Reflectivity-VariableBreakpoint' }
Class >> newBreakpointForVariable: aVariableName [
	^ VariableBreakpoint
		newBreakpointForVariable: aVariableName
		inClass: self
]
