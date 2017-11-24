"
I clusterize CompiledMethods that will be obtained on materialization accessing method dictionary of the corresponding class in Smalltalk globals.

"
Class {
	#name : #FLGlobalCompiledMethodCluster,
	#superclass : #FLGlobalCluster,
	#category : #'Fuel-Clusters'
}

{ #category : #'serialize/materialize' }
FLGlobalCompiledMethodCluster >> materializeInstanceWith: aDecoder [

	| methodClass selector method serializedHash |
	methodClass := self materializeGlobalClassFrom: aDecoder.
	(aDecoder nextEncodedByte = 1) ifTrue: [
		methodClass := methodClass classSide].
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
	| methodClass |
	methodClass := aCompiledMethod methodClass.
	self serializeGlobalClass: methodClass instanceSide on: anEncoder.
	anEncoder encodeByte: (methodClass isClassSide ifTrue: [1] ifFalse: [0]).
	anEncoder encodeString: aCompiledMethod selector.
	anEncoder encodeUint16: aCompiledMethod bytecodesHash.
]
