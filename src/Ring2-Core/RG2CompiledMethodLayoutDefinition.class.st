Class {
	#name : #RG2CompiledMethodLayoutDefinition,
	#superclass : #RG2ObjectLayoutDefinition,
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'testing types' }
RG2CompiledMethodLayoutDefinition >> isBitsLayout [

	^ true
]

{ #category : #'testing types' }
RG2CompiledMethodLayoutDefinition >> isByteLayout [

	^ true
]

{ #category : #'testing types' }
RG2CompiledMethodLayoutDefinition >> isCompiledMethodLayout [

	^ true
]

{ #category : #'testing types' }
RG2CompiledMethodLayoutDefinition >> isVariableLayout [

	^ true
]
