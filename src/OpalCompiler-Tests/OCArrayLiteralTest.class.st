Class {
	#name : #OCArrayLiteralTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #running }
OCArrayLiteralTest >> runCase [

	SystemAnnouncer uniqueInstance suspendAllWhile: [ super runCase ]

]

{ #category : #running }
OCArrayLiteralTest >> tearDown [

	self class removeSelector: #array.
	super tearDown
]

{ #category : #tests }
OCArrayLiteralTest >> testByteArrayBase [
	self class compile: 'array ^ #[2r1010 8r333 16rFF]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: (self array size = 3).
	self assert: (self array first = 10).
	self assert: (self array second = 219).
	self assert: (self array last = 255)
	

]

{ #category : #tests }
OCArrayLiteralTest >> testByteArrayEmpty [
	self class compile: 'array ^ #[]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: (self array isEmpty)
]

{ #category : #tests }
OCArrayLiteralTest >> testByteArrayLiteral [
	self class compile: 'array ^ #[ 1 2 3 4 ]'.
	self assert: (self array = self array).
	self assert: (self array == self array)
]

{ #category : #tests }
OCArrayLiteralTest >> testByteArrayLong [
	self class compile: 'array ^ #[ ' , ((0 to: 255) inject: ' ' into: [ :r :e | r , ' ' , e asString ]) , ' ]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: (self array size = 256).
	0 to: 255 do: [ :index | self assert: (self array at: index + 1) = index ]
]

{ #category : #tests }
OCArrayLiteralTest >> testByteArrayRange [
	self class compile: 'array ^ #[ 0 255 ]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: (self array size = 2).
	self assert: (self array first = 0).
	self assert: (self array last = 255)
]

{ #category : #tests }
OCArrayLiteralTest >> testByteArrayWithinArray [
	self class compile: 'array ^ #( #[1] #[2] )'.
	self assert: (self array isKindOf: Array).
	self assert: (self array size = 2).
	self assert: (self array first isKindOf: ByteArray).
	self assert: (self array first first = 1).
	self assert: (self array last isKindOf: ByteArray).
	self assert: (self array last first = 2)
	

]

{ #category : #tests }
OCArrayLiteralTest >> testReservedIdentifiers [
	self class compile: 'array ^ #(nil true false)'.
	self assert: self array = {nil. true. false}.
]

{ #category : #tests }
OCArrayLiteralTest >> testSymbols [
	self class compile: 'array ^ #(#nil #true #false #''nil'' #''true'' #''false'')'.
	self assert: self array = {#nil. #true. #false. #nil. #true. #false}.
]
