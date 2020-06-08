Extension { #name : #CompiledBlock }

{ #category : #'*Fuel-Platform-Pharo-Core' }
CompiledBlock >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitCompiledBlock: self
]

{ #category : #'*Fuel-Platform-Pharo-Core' }
CompiledBlock >> fuelPrepare [
	"Nothing to do here"
]

{ #category : #'*Fuel-Platform-Pharo-Core' }
CompiledBlock >> isEqualRegardlessTrailerTo: aCompiledBlock [
	^ self = aCompiledBlock
]
