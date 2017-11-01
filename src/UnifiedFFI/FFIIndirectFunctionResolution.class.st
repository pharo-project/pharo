"
I will perform an ""indirect function call"" as explained here: https://en.wikipedia.org/wiki/Function_pointer

In summary: I take a function pointer kept in a global symbol and execute it. 

I implement the option:  optIndirectCall 
"
Class {
	#name : #FFIIndirectFunctionResolution,
	#superclass : #FFIFunctionResolutionStrategy,
	#category : 'UnifiedFFI'
}

{ #category : #testing }
FFIIndirectFunctionResolution >> isApplicableFor: aRequestor [
	^ aRequestor optionAt: #optIndirectCall
]

{ #category : #resolving }
FFIIndirectFunctionResolution >> resolveFunction: anExternalFunction [
	^ (ExternalAddress 
		loadSymbol: anExternalFunction name
		module: anExternalFunction module)
		pointerAt: 1.
]
