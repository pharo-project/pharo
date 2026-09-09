Extension { #name : 'VirtualMachine' }

{ #category : '*System-Version' }
VirtualMachine >> imageVersionInImageHeader [

	^ self parameterAt: 79
]

{ #category : '*System-Version' }
VirtualMachine >> imageVersionInImageHeader: aValue [

	(aValue between: 0 and: 65535)
		ifFalse: [ self error: 'The image version should be an integer positive of 16bits' ].
	
	^ self parameterAt: 79 put: aValue
]

{ #category : '*System-Version' }
VirtualMachine >> imageVersionNumber [
	"Image version number (6505 means the Squeak V3 format with BlockClosure support (but _without_ BlockContext support)"

	^ self parameterAt: 41
]

{ #category : '*System-Version' }
VirtualMachine >> saveImageVersionInImageHeader [

	| currentVersion |
	currentVersion := SystemVersion current.

	"If the primitive is not available we return false"
	[	self imageVersionInImageHeader: (currentVersion major * 10) + currentVersion minor.
		^ true ]
		onErrorDo: [ ^ false ]. 
		
]
