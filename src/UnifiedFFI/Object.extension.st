Extension { #name : #Object }

{ #category : #'*UnifiedFFI' }
Object >> calloutAPIClass [
	^ self ffiLibrary calloutAPIClass
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec [
	<ffiCalloutTranslator>
	
	"All the ffiCall: messages are used to perform Foreign-Function calls (FFI calls).
	FFI calls allows Pharo code to interact with external dynamic linked libraries.
	Please refer to the booklet about more configuration details
	or some of the examples on the image LibC / FreeTypeFont / Athens / SDL."
	
	self ffiCall: fnSpec library: self ffiLibrary
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec library: aLibrary [
	<ffiCalloutTranslator>
	
	self ffiCall: fnSpec library: aLibrary options: #()
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec library: aLibrary options: callOptions [
	<ffiCalloutTranslator>
	
	| ffiLibrary |
	ffiLibrary := aLibrary asFFILibrary.
	^ (ffiLibrary calloutAPIClass inUFFIContext: thisContext)
		convention: self ffiCallingConvention;
		options: (ffiLibrary options), callOptions;
		function: fnSpec library: ffiLibrary
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec module: aModuleName [
	<ffiCalloutTranslator>
	
	self ffiCall: fnSpec library: aModuleName
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec module: aModuleName options: callOptions [
	<ffiCalloutTranslator>
	
	self ffiCall: fnSpec library: aModuleName options: callOptions
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec options: callOptions [
	<ffiCalloutTranslator>
	
	self ffiCall: fnSpec library: self ffiLibrary libraryName options: callOptions
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCallingConvention [
	^ OSPlatform current ffiCallingConvention
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCalloutIn: aContext [
	"For backwards compatibility.
	It receives the context that is performing the ffi callout.
	This context will be used to know what method to compile and where to restart the execution"
	^ self calloutAPIClass inContext: aContext
]

{ #category : #'*UnifiedFFI' }
Object >> ffiLibrary [
	
	"This method returns the ffiLibrary to use in the Foreign-Function calls performed in this object.
	FFI calls allows Pharo to call functions in other external dynamic linked libraries.
	
	See #ffiCall: to see how to perform a FFI call.
	Subclasses can redefine this method to use a different FFILibrary class (see class FFILibrary). 
	A FFILibrary object not only returns the path to the dynamic linked library to use (DLL/SO/DYLIB)
	but it can also configure different behaviours for the FFI Calls. For example, the backend to use 
	or how the types are mapped. 
	
	This default implementation is kept as backward compatibility. 
	Its implementation only returns a default configured FFILibrary using the path to the 
	dynamic library file returned by #ffiLibraryName 
	
	New users of FFI should return here a subclass of FFILibrary configured as required by the target 
	external library to call by FFI"
		
	^ self ffiLibraryName asFFILibrary
]

{ #category : #'*UnifiedFFI' }
Object >> ffiLibraryName [
	self flag: 'HACK: avoid direct subclassResponsibility to not break RB test cases..'.
	^ SubclassResponsibility signalFor: thisContext selector
]

{ #category : #'*UnifiedFFI' }
Object >> isExternalType [
	^ false
]

{ #category : #'*UnifiedFFI' }
Object >> newCallbackWithSignature: signature block: aBlock [
	"This method is used to create a callback in the same library and callout api of the receiver"
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		newCallbackWithSignature: signature
			block: aBlock
			library: self ffiLibrary
]

{ #category : #'*UnifiedFFI' }
Object >> newCallbackWithSignature: signature block: aBlock library: aFFILibrary [
	"This method is used to create a callback in the callout api of the receiver"
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		newCallbackWithSignature: signature
			block: aBlock
			library: aFFILibrary
]

{ #category : #'*UnifiedFFI' }
Object >> packToArity: arity [
	"This will answer a pointer to this object. 
	 It is like doing ==var ptr=&aVariable== in C (up to arity). 
	
	 In general, arity>1 will not work because then object has to be copied to 
	 heap, but my overrides will handle this case"
	| rolledPointer |
		
	rolledPointer := self.
	1 to: arity do: [ :index | rolledPointer := rolledPointer pointer ].
	^ rolledPointer
]

{ #category : #'*UnifiedFFI' }
Object >> pointer [
	"Answers a pointer to me (see overrides for implementations)"
	self error: 'You cannot get a pointer to me.'
]
