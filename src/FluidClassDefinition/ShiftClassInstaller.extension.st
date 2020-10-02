Extension { #name : #ShiftClassInstaller }

{ #category : #'*FluidClassDefinition' }
ShiftClassInstaller >> makeWithBuilder: aBuilder [
	
	builder := aBuilder.
	^ self make
]
