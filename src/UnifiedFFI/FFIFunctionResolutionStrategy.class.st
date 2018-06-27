"
I define a strategy of function resolution for doing callouts. 
By default, functions are obtained in first execution of it, in VM side (it just do a lookup of the symbol, and installs it in the ExternalLibraryFunction literal (no point in doing it every time). 
Sometimes we need to obtain this function pointers in different ways (for example when doing indirect calls: accessing functions installed in a global symbol, instead the real name of it). 

My children will implement different strategies to obtain this function pointers. They need to provide two methods: 

isApplicableFor:  ""answers true if function should be resolved using this strategy""
resolveFunctionAddress: ""answers the solved function address (an ExternalAddress)""
"
Class {
	#name : #FFIFunctionResolutionStrategy,
	#superclass : #Object,
	#category : #'UnifiedFFI-Callouts'
}

{ #category : #testing }
FFIFunctionResolutionStrategy >> isApplicableFor: aRequestor [
	"answers true if function should be resolved using this strategy"
	self subclassResponsibility
]

{ #category : #resolving }
FFIFunctionResolutionStrategy >> resolve: anExternalFunction [
	anExternalFunction setHandle: (self resolveFunction: anExternalFunction)
]

{ #category : #resolving }
FFIFunctionResolutionStrategy >> resolveFunction: anExternalFunction [
	"answers the solved function address (an ExternalAddress)"
	self subclassResponsibility
]
