Extension { #name : #CompiledMethod }

{ #category : #'*Reflectivity-VariableBreakpoint' }
CompiledMethod >> link: aMetaLink toVariableNamed: vName option: accessStrategy [
	^ self methodClass
		link: aMetaLink
		toTemporaryNamed: vName
		inMethod: self selector
		option: accessStrategy
]

{ #category : #'*Reflectivity-VariableBreakpoint' }
CompiledMethod >> newBreakpointForVariable: aVariableName [
	^ (self tempNames includes: aVariableName)
		ifTrue: [ VariableBreakpoint
				newBreakpointForVariable: aVariableName
				forMethod: self ]
		ifFalse: [ VariableBreakpoint
				newBreakpointForVariable: aVariableName
				inClass: self methodClass ]
]
