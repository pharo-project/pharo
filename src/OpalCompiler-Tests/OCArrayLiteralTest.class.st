Class {
	#name : 'OCArrayLiteralTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-FromOld',
	#package : 'OpalCompiler-Tests',
	#tag : 'FromOld'
}

{ #category : 'running' }
OCArrayLiteralTest >> compile: methodSourceCode [

	^ OpalCompiler new
		source: methodSourceCode;
		compileOrFail	
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayBase [
	
	| array method | 
	method := self compile: 'array ^ #[2r1010 8r333 16rFF]'.
	array := method valueWithReceiver: nil.
	self assert: (array isKindOf: ByteArray).
	self assert: array size equals: 3.
	self assert: array first equals: 10.
	self assert: array second equals: 219.
	self assert: array last equals: 255
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayEmpty [

	| array method |
	method := self compile: 'array ^ #[]'.
	array := method valueWithReceiver: nil.
	self assert: (array isKindOf: ByteArray).
	self assertEmpty: array
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayLiteral [

	| array method |
	method := self compile: 'array ^ #[ 1 2 3 4 ]'.
	array := method valueWithReceiver: #().
	self assert: array equals: array.
	self assert: array identicalTo: array
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayLong [
	
	| method array |
	method := self compile: 'array ^ #[ ' , ((0 to: 255) inject: ' ' into: [ :r :e | r , ' ' , e asString ]) , ' ]'.
	array := method valueWithReceiver: #().
	
	self assert: ( array isKindOf: ByteArray).
	self assert: array size equals: 256.
	0 to: 255 do: [ :index | self assert: (array at: index + 1) equals: index ]
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayRange [
	
	| array method |
	method := self compile: 'array ^ #[ 0 255 ]'.
	array := method valueWithReceiver: #().
	self assert: ( array isKindOf: ByteArray).
	self assert:  array size equals: 2.
	self assert:  array first equals: 0.
	self assert:  array last equals: 255
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayWithinArray [

	| array method |
	method := self compile: 'array ^ #( #[1] #[2] )'.
	array := method valueWithReceiver: #().
	self assert: (array isKindOf: Array).
	self assert: array size equals: 2.
	self assert: (array first isKindOf: ByteArray).
	self assert: array first first equals: 1.
	self assert: (array last isKindOf: ByteArray).
	self assert:  array last first equals: 2
]

{ #category : 'tests' }
OCArrayLiteralTest >> testLiteralArrayLongHavingAnotherArrayLong [

	| method array elements |
	elements := ((1 to: 32) inject: ' ' into: [ :r :e | r , ' . ' , e asString ]).
	array := ' { ' , elements , ' }'.
	array := array copyReplaceAll: '32' with: array.
	
	method := self compile: 'array ^' , array.
	array := method valueWithReceiver: #().
	
	self assert: ( array isKindOf: Array).
	self assert: array size equals: 32.
	1 to: 31 do: [ :index | self assert: (array at: index) equals: index ].
	1 to: 32 do: [ :index | self assert: ((array at: 32) at: index) equals: index ].
]

{ #category : 'tests' }
OCArrayLiteralTest >> testReservedIdentifiers [

	| method array |
	method := self compile: 'array ^ #(nil true false)'.
	array := method valueWithReceiver: #(). 
	self 
		assert: array 
		equals: {nil . true . false}
]

{ #category : 'tests' }
OCArrayLiteralTest >> testSymbols [
	
	| array method |
	method := self compile: 'array  ^ #(#nil #true #false #''nil'' #''true'' #''false'')'.
	array := method valueWithReceiver: #().
	
	self 
		assert: array 
		equals: { #nil . #true . #false . #nil . #true . #false }
]
