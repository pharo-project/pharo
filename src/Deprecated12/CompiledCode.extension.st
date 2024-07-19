Extension { #name : 'CompiledCode' }

{ #category : '*Deprecated12' }
CompiledCode >> bytecode [
	self
		deprecated: 'Changed selector for coherency'
		transformWith: '`@receiver bytecode'
						-> '`@receiver bytecodes'.
	^ self bytecodes
]

{ #category : '*Deprecated12' }
CompiledCode >> packageFromOrganizer: aPackageOrganizer [

	self
		deprecated: 'Use #package instead since the previous implementation was not using the parameter.'
		transformWith: '`@rcv packageFromOrganizer: `@arg' -> '`@rcv package'.
	^ self package
]
