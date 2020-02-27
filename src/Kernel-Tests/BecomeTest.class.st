Class {
	#name : #BecomeTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Objects'
}

{ #category : #testing }
BecomeTest >> testBecome [
	"Test the two way become. Note. we cannot use string literals for this test"
	| a b c d |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	d := b.

	a become: b.

	self 
		assert: a equals: 'cd';
		assert: b equals: 'ab';
		assert: c equals: 'cd';
		assert: d equals: 'ab'.



]

{ #category : #testing }
BecomeTest >> testBecomeForward [
	"Test the forward become."
	| a b c d |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	d := b.

	a becomeForward: b.

	self 
		assert: a equals: 'cd';
		assert: b equals: 'cd';
		assert: c equals: 'cd';
		assert: d equals: 'cd'.



]

{ #category : #testing }
BecomeTest >> testBecomeForwardCopyIdentityHash [
	"Check that
		1. the argument to becomeForward: IS modified to have the sender's identity hash.
		2. the sender's identity hash is unchanged."

	| a b identityHashOfA |
	a := 'ab' copy.
	b := 'cd' copy.
	identityHashOfA := a identityHash.

	a becomeForward: b copyHash: true.

	self
		assert: a identicalTo: b;
		assert: a identityHash equals: identityHashOfA;
		assert: b identityHash equals: identityHashOfA
]

{ #category : #testing }
BecomeTest >> testBecomeForwardDontCopyIdentityHash [
	"Check that
		1. the argument to becomeForward: is NOT modified to have the receiver's identity hash.
		2. the receiver's identity hash is unchanged."

	| a b identityHashOfB |
	a := 'ab' copy.
	b := 'cd' copy.
	identityHashOfB := b identityHash.

	a becomeForward: b copyHash: false.

	self
		assert: a identicalTo: b;
		assert: a identityHash equals: identityHashOfB;
		assert: b identityHash equals: identityHashOfB
]

{ #category : #testing }
BecomeTest >> testBecomeForwardHash [

	| a b c hb |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	hb := b hash.

	a becomeForward: b.

	self 
		assert: a hash equals: hb;
		assert: b hash equals: hb;
		assert: c hash equals: hb.



]

{ #category : #testing }
BecomeTest >> testBecomeForwardIdentityHash [
	"Check that
		1. the argument to becomeForward: is modified to have the receiver's identity hash.
		2. the receiver's identity hash is unchanged."

 	| a b ha |

	a := 'ab' copy.
	b := 'cd' copy.
	ha := a identityHash.

	a becomeForward: b.

	self 
		assert: a identityHash equals: ha;
		assert: b identityHash equals: ha.


]

{ #category : #testing }
BecomeTest >> testBecomeHash [

	| a b c d ha hb |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	d := b.
	ha := a hash.
	hb := b hash.

	a become: b.

	self 
		assert: a hash equals: hb;
		assert: b hash equals: ha;
		assert: c hash equals: hb;
		assert: d hash equals: ha



]

{ #category : #testing }
BecomeTest >> testBecomeIdentityHash [
	"Note. The identity hash of both objects seems to change after the become:"

	| a b c d |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	d := b.

	a become: b.

	self 
		assert: a identityHash equals: c identityHash;
		assert: b identityHash equals: d identityHash;
		deny: a identityHash equals: b identityHash.

]
