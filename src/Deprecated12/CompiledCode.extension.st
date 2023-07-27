Extension { #name : #CompiledCode }

{ #category : #'*Deprecated12' }
CompiledCode >> packageFromOrganizer: anRPackageOrganizer [

	self deprecated: 'Use #package instead since the parameter was never used.' transformWith: '`@rcv packageFromOrganizer: `@arg' -> '`@rcv package'.
	^ self package
]
