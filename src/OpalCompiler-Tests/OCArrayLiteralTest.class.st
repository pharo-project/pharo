Class {
	#name : 'OCArrayLiteralTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-FromOld',
	#package : 'OpalCompiler-Tests',
	#tag : 'FromOld'
}

{ #category : 'running' }
OCArrayLiteralTest >> runCase [

	self class codeChangeAnnouncer suspendAllWhile: [ super runCase ]
]

{ #category : 'running' }
OCArrayLiteralTest >> tearDown [

	self class removeSelector: #array.
	super tearDown
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayBase [
	self class compile: 'array ^ #[2r1010 8r333 16rFF]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: self array size equals: 3.
	self assert: self array first equals: 10.
	self assert: self array second equals: 219.
	self assert: self array last equals: 255
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayEmpty [
	self class compile: 'array ^ #[]'.
	self assert: (self array isKindOf: ByteArray).
	self assertEmpty: self array
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayLiteral [
	self class compile: 'array ^ #[ 1 2 3 4 ]'.
	self assert: self array equals: self array.
	self assert: self array identicalTo: self array
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayLong [
	self class compile: 'array ^ #[ ' , ((0 to: 255) inject: ' ' into: [ :r :e | r , ' ' , e asString ]) , ' ]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: self array size equals: 256.
	0 to: 255 do: [ :index | self assert: (self array at: index + 1) equals: index ]
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayRange [
	self class compile: 'array ^ #[ 0 255 ]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: self array size equals: 2.
	self assert: self array first equals: 0.
	self assert: self array last equals: 255
]

{ #category : 'tests' }
OCArrayLiteralTest >> testByteArrayWithinArray [
	self class compile: 'array ^ #( #[1] #[2] )'.
	self assert: (self array isKindOf: Array).
	self assert: self array size equals: 2.
	self assert: (self array first isKindOf: ByteArray).
	self assert: self array first first equals: 1.
	self assert: (self array last isKindOf: ByteArray).
	self assert: self array last first equals: 2
]

{ #category : 'tests' }
OCArrayLiteralTest >> testReservedIdentifiers [
	self class compile: 'array ^ #(nil true false)'.
	self assert: self array equals: {nil . true . false}
]

{ #category : 'tests' }
OCArrayLiteralTest >> testSymbols [
	self class compile: 'array ^ #(#nil #true #false #''nil'' #''true'' #''false'')'.
	self assert: self array equals: {#nil . #true . #false . #nil . #true . #false}
]
