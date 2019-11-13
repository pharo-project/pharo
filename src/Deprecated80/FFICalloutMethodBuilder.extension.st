Extension { #name : #FFICalloutMethodBuilder }

{ #category : #'*Deprecated80' }
FFICalloutMethodBuilder >> moduleName [
	self
		deprecated: 'Please use #libraryName instead'
		transformWith: '`@receiver moduleName' -> '`@receiver libraryName'.

	^self libraryName
]
