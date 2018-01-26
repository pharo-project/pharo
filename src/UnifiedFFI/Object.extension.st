Extension { #name : #Object }

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec [
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec module: self ffiLibraryName
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec module: aModuleName [
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec module: aModuleName
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec module: aModuleName options: callOptions [
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec module: aModuleName
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCall: fnSpec options: callOptions [
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec module: self ffiLibraryName
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCallingConvention [
	^ OSPlatform current ffiCallingConvention
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCallout [
	^ FFICalloutAPI calloutAPIClass inContext: thisContext sender
]

{ #category : #'*UnifiedFFI' }
Object >> ffiCalloutIn: aContext [
	^ FFICalloutAPI calloutAPIClass inContext: aContext
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
