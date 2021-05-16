"
I clusterize CompiledMethods that will be obtained on materialization accessing method dictionary of the corresponding class in Smalltalk globals.

"
Class {
	#name : #FLGlobalCompiledMethodCluster,
	#superclass : #FLGlobalCompiledCodeCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLGlobalCompiledMethodCluster >> materializeInstanceWith: aDecoder [
	| methodClass selector method serializedHash |
	methodClass := self materializeGlobalClassFrom: aDecoder.
	selector := aDecoder nextEncodedString asSymbol.
	method := methodClass  
		compiledMethodAt: selector
		ifAbsent: [FLMethodNotFound signalWith: methodClass name and: selector].
	serializedHash := aDecoder nextEncodedUint16.
	method bytecodesHash =  serializedHash
		ifFalse: [FLMethodChanged signalWith: methodClass name and: selector].
	^method
]

{ #category : #'serialize/materialize' }
FLGlobalCompiledMethodCluster >> serializeInstance: aCompiledMethod with: anEncoder [
	self
		serializeGlobalClass: aCompiledMethod methodClass
		on: anEncoder.
	anEncoder
		encodeString: aCompiledMethod selector;
		encodeUint16: aCompiledMethod bytecodesHash
]
