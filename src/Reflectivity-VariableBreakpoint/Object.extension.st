Extension { #name : #Object }

{ #category : #'*Reflectivity-VariableBreakpoint' }
Object >> haltOnAccess [
	^ VariableBreakpoint watchVariablesInObject: self
]

{ #category : #'*Reflectivity-VariableBreakpoint' }
Object >> haltOnAccessTo: aVariableName [
	^VariableBreakpoint watchVariable: aVariableName inObject: self
]

{ #category : #'*Reflectivity-VariableBreakpoint' }
Object >> haltOnRead [
	^VariableBreakpoint watchVariablesReadsInObject: self
]

{ #category : #'*Reflectivity-VariableBreakpoint' }
Object >> haltOnReadTo: aVariableName [
	^VariableBreakpoint watchVariableReads: aVariableName inObject: self
]

{ #category : #'*Reflectivity-VariableBreakpoint' }
Object >> haltOnWrite [
	^VariableBreakpoint watchVariablesWritesInObject: self
]

{ #category : #'*Reflectivity-VariableBreakpoint' }
Object >> haltOnWriteTo: aVariableName [
	^VariableBreakpoint watchVariableWrites: aVariableName inObject: self
]
