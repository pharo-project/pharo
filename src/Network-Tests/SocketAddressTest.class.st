"
Unit test for SocketAddress
"
Class {
	#name : #SocketAddressTest,
	#superclass : #TestCase,
	#category : #'Network-Tests-Kernel'
}

{ #category : #testing }
SocketAddressTest >> testByteArrayConversionErrors [
	self should: [ #[ ] asSocketAddress ] raise: Error.
	self should: [ #[ 1 ] asSocketAddress ] raise: Error.
	self should: [ #[ 1 2 ] asSocketAddress ] raise: Error.
	self should: [ #[ 1 2 3 ] asSocketAddress ] raise: Error.
	self should: [ #[ 1 2 3 4 5] asSocketAddress ] raise: Error
]

{ #category : #testing }
SocketAddressTest >> testDottedRepresentation [
	| address |
	address := SocketAddress fromDottedString: '104.28.27.35'.
	self assert: address equals: #[ 104 28 27 35 ] asSocketAddress.
	self assert: address printString equals: '104.28.27.35'
]

{ #category : #testing }
SocketAddressTest >> testDottedRepresentationErrors [
	self should: [ SocketAddress fromDottedString: '' ] raise: Error.
	self should: [ SocketAddress fromDottedString: '1' ] raise: Error.
	self should: [ SocketAddress fromDottedString: '1.2' ] raise: Error.
	self should: [ SocketAddress fromDottedString: '1.2.3' ] raise: Error.
	self should: [ SocketAddress fromDottedString: '1.2.3.4.5' ] raise: Error.
	self should: [ SocketAddress fromDottedString: '1.2.3.300' ] raise: Error.
	self should: [ SocketAddress fromDottedString: '1 . 2 . 3 . 200' ] raise: Error
]

{ #category : #testing }
SocketAddressTest >> testIsPrivateAddress [
	#('10.0.0.1' '172.20.1.1' '192.168.10.12') do: [ :each | 
		self assert: (SocketAddress fromDottedString: each) isPrivateAddress ].
	#('127.0.0.1' '172.33.1.1' '104.28.27.35' '0.0.0.0') do: [ :each | 
		self deny: (SocketAddress fromDottedString: each) isPrivateAddress ]
	
]

{ #category : #testing }
SocketAddressTest >> testZero [
	| address |
	address := SocketAddress zero.
	self assert: address size equals: 4.
	address do: [ :each | self assert: each isZero ].
	self assert: address equals: (SocketAddress fromDottedString: '0.0.0.0').
	self assert: address equals: (#[0 0 0 0] asSocketAddress).
	self assert: address printString equals: '0.0.0.0'
]
