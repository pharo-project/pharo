"
I clusterize CompiledMethods that will be obtained on materialization accessing method dictionary of the corresponding class in Smalltalk globals.

"
Class {
	#name : 'FLGlobalCompiledMethodCluster',
	#superclass : 'FLGlobalCompiledCodeCluster',
	#category : 'Fuel-Core-Clusters-Global',
	#package : 'Fuel-Core',
	#tag : 'Clusters-Global'
}

{ #category : 'serialize/materialize' }
FLGlobalCompiledMethodCluster >> materializeInstanceWith: aDecoder [
	| methodClass selector method serializedHash |
	methodClass := self materializeGlobalClassFrom: aDecoder.
	selector := aDecoder nextEncodedString asSymbol.
	method := self findMethodWithSelector: selector in: methodClass.
	serializedHash := aDecoder nextEncodedUint16.
	aDecoder configuration isMethodChangedWarningDisabled ifFalse: [
		method bytecodesHash = serializedHash ifFalse: [
			FLMethodChanged
				signalWith: methodClass name
				and: selector ] ].
	^ method
]

{ #category : 'serialize/materialize' }
FLGlobalCompiledMethodCluster >> serializeInstance: aCompiledMethod with: anEncoder [
	self
		serializeGlobalClass: aCompiledMethod methodClass
		on: anEncoder.
	anEncoder
		encodeString: aCompiledMethod selector;
		encodeUint16: aCompiledMethod bytecodesHash
]
