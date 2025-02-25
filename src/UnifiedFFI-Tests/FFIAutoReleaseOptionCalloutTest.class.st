Class {
	#name : 'FFIAutoReleaseOptionCalloutTest',
	#superclass : 'TestCase',
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'instance creation' }
FFIAutoReleaseOptionCalloutTest >> newFFIMethodWithSignature: signature doing: aBlock options: options [

	| calloutAPI methodBuilder |
	calloutAPI := FFICalloutAPI inUFFIContext: nil.
	methodBuilder := FFIMockCalloutMethodBuilder calloutAPI: calloutAPI.
	methodBuilder doing: aBlock.
	methodBuilder requestor: (FFICallout new
		methodArgs: aBlock argumentNames;
		requestor: methodBuilder;
		options: options;
		yourself).

	^ methodBuilder build: [ :builder |
			builder
				signature: signature;
				sender: methodBuilder ]
]

{ #category : 'tests' }
FFIAutoReleaseOptionCalloutTest >> testExternalObject [

	self
		testOnFFIMethodWithSignature: #( FFITestObject f #(  ) )
		doing: [ ExternalAddress new ]
		checkWith: [ :return |
			FFIExternalResourceManager uniqueInstance includes: return ]
]

{ #category : 'tests' }
FFIAutoReleaseOptionCalloutTest >> testExternalStructure [

	self
		testOnFFIMethodWithSignature: #( FFITestStructure * f ( ) )
		doing: [ FFITestStructure externalNew ]
		checkWith: [ :return |
			ExternalAddress finalizationRegistry includes: return getHandle ]
]

{ #category : 'tests' }
FFIAutoReleaseOptionCalloutTest >> testNotYetImplementedOnString [

	self
		should: [
			self
				newFFIMethodWithSignature: #( char* f ( ) )
				doing: [ ExternalAddress null ]
				options: #( + optAutoReleaseReturnedValue ) ]
		raise: Error
		whoseDescriptionIncludes: 'cannot yet use +optAutoReleaseReturnedValue'
		description: 'Not yet implemented'
]

{ #category : 'tests' }
FFIAutoReleaseOptionCalloutTest >> testOnFFIMethodWithSignature: anArray doing: mockedFunctionReturn checkWith: aBlockToTestIfAutoReleaseWasDone [

	| return method |
	method := self
		newFFIMethodWithSignature: anArray
		doing: mockedFunctionReturn
		options: #().
	return := method valueWithReceiver: nil.
	self
		deny: (aBlockToTestIfAutoReleaseWasDone value: return)
		description: 'By default, autoRelease was not performed on the returned object'.

	method := self
		newFFIMethodWithSignature: anArray
		doing: mockedFunctionReturn
		options: #( + optAutoReleaseReturnedValue ).
	return := method valueWithReceiver: nil.
	self
		assert: (aBlockToTestIfAutoReleaseWasDone value: return)
		description: 'With the option, autoRelease was performed'
]

{ #category : 'tests' }
FFIAutoReleaseOptionCalloutTest >> testVoidPointer [

	self
		testOnFFIMethodWithSignature: #( void* f ( ) )
		doing: [ ExternalAddress null ]
		checkWith: [ :return |
			ExternalAddress finalizationRegistry includes: return ]
]
