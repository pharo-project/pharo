Extension { #name : #CompiledBlock }

{ #category : #'*Fuel-Core' }
CompiledBlock >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitCompiledBlock: self
]
