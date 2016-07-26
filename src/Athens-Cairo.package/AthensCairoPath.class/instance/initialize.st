initialize-release
initialize 
	handle value = 0 ifTrue: [ 
		self error: 'Error creating new path' ].
	FFIExternalResourceManager addResource: self data: handle