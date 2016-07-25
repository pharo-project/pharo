initialize-release
initialize
	handle value = 0 
		ifTrue: [ self error: 'Error creating new paint' ].
	"register to be finalized"
	FFIExternalResourceManager addResource: self data: handle.