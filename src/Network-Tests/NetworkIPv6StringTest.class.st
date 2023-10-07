Class {
	#name : 'NetworkIPv6StringTest',
	#superclass : 'TestCase',
	#category : 'Network-Tests-Kernel',
	#package : 'Network-Tests',
	#tag : 'Kernel'
}

{ #category : 'tests' }
NetworkIPv6StringTest >> testCompressedZerosOnlyToString [
	"Ensure that a ByteArray of size 16 consisting of all zeros is
	correctly represented as just the compressed zero IPv6 string (::)"
	| source expected result |
	source := #[ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ].
	expected := '::'.
	result := NetNameResolver stringFromIPv6Address: source.

	self assert: expected equals: result
]

{ #category : 'tests' }
NetworkIPv6StringTest >> testCompressedZerosOnlytoBytes [
	"Ensure that an IPv6 string containing only compressed zeroes
	(::) responds with a ByteArray of all zeros."
	| source expected result |
	source := '::'.
	expected := #[ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ].
	result := NetNameResolver addressFromIPv6String: source.

	self assert: expected equals: result
]

{ #category : 'tests' }
NetworkIPv6StringTest >> testHeadCompressedZerosToBytes [
	"Ensure that compressed zeros (::) at the start of the
	IPv6 string are accurately represented in the resulting ByteArray"
	| source expected result |
	source := '::2001:db8'.
	expected := #[ 0 0 0 0 0 0 0 0 0 0 0 0 32 1 13 184 ].
	result := NetNameResolver addressFromIPv6String: source.

	self assert: expected equals: result
]

{ #category : 'tests' }
NetworkIPv6StringTest >> testHeadCompressedZerosToString [
	"Ensure that a valid 16-size ByteArray corresponding
	to an IPv6 address that begins with some number of
	compressed zeros accurately responds with a string
	that begins with the compressed symbol (::)."
	| source expected result |
	source := #[ 0 0 0 0 0 0 0 0 0 0 0 0 32 1 13 184 ].
	expected := '::2001:db8'.
	result := NetNameResolver stringFromIPv6Address: source.

	self assert: expected equals: result
]

{ #category : 'tests' }
NetworkIPv6StringTest >> testMiddleCompressedZeroToBytes [
	"Ensure that the compressed zeros, represented by '::' in
	the source IPv6 string, are accurately written out to the
	resulting ByteArray."
	| source expected result |
	source := '2001:db8::1:0:0:1'.
	expected := #[32 1 13 184 0 0 0 0 0 1 0 0 0 0 0 1].
	result := NetNameResolver addressFromIPv6String: source.

	self assert: expected equals: result
]

{ #category : 'tests' }
NetworkIPv6StringTest >> testMiddleCompressedZeroToString [
	"Ensure that a size-16 ByteArray corresponding to an IPv6 address
	that contains some middle range of consecutive zeros responds with
	a string that includes the consecutive zero substring (::) in the correct place."
	| source expected result |
	source := #[32 1 13 184 0 0 0 0 0 1 0 0 0 0 0 1].
	expected := '2001:db8::1:0:0:1'.
	result := NetNameResolver stringFromIPv6Address: source.

	self assert: expected equals: result
]

{ #category : 'tests' }
NetworkIPv6StringTest >> testTailCompressedZerosToBytes [
	"Ensure that compressed zeroes (::) at the tail of a valid
	IPv6 string are accurately represented in the resulting ByteArray."
	| source expected result |
	source := 'db8:2001::'.
	expected := #[ 13 184 32 1 0 0 0 0 0 0 0 0 0 0 0 0 ].
	result := NetNameResolver addressFromIPv6String: source.

	self assert: expected equals: result
]

{ #category : 'tests' }
NetworkIPv6StringTest >> testTailCompressedZerosToString [
	"Ensure that a size 16 ByteArray corresponding to an IPv6 address
	that ends in a contiguous range of zeros has the correct compressed
	zero substring (::) at the end of the string."
	| source expected result |
	source := #[ 13 184 32 1 0 0 0 0 0 0 0 0 0 0 0 0 ].
	expected := 'db8:2001::'.
	result := NetNameResolver stringFromIPv6Address: source.

	self assert: expected equals: result
]
