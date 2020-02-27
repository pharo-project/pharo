Extension { #name : #InstanceVariableSlot }

{ #category : #'*Ring-Core' }
InstanceVariableSlot >> asRingMinimalDefinitionIn: anRGEnvironment [
	
	^ self class == InstanceVariableSlot 
		ifTrue: [
			anRGEnvironment backend definitionFor: self ifAbsentRegister: [
				RGInstanceVariableSlot
					named: self name asSymbol
					parent:
						(self definingClass classLayout asRingMinimalDefinitionIn: anRGEnvironment)]]
		ifFalse: [ super asRingMinimalDefinitionIn: anRGEnvironment ]


]
