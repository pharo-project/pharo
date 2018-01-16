Extension { #name : #InstanceVariableSlot }

{ #category : #'*Ring2-Core' }
InstanceVariableSlot >> asRing2MinimalDefinitionIn: aRG2Environment [
	
	^ self class == InstanceVariableSlot 
		ifTrue: [
			aRG2Environment backend definitionFor: self ifAbsentRegister: [
				RG2InstanceVariableSlotDefinition
					named: self name asSymbol
					parent:
						(self definingClass classLayout asRing2MinimalDefinitionIn: aRG2Environment)]]
		ifFalse: [ super asRing2MinimalDefinitionIn: aRG2Environment ]


]
