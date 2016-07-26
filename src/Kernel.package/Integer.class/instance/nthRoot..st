mathematical functions
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver.
	Answer an Integer if root is exactly this Integer, else answer the Float nearest the exact root."

	| guess p |

	guess := self nthRootRounded: aPositiveInteger.
	(guess raisedTo: aPositiveInteger) = self
		ifTrue: [ ^ guess ].

	p := Float precision - guess highBitOfMagnitude.
	p < 0 ifTrue: [ ^ guess asFloat ].

	guess := self << (p * aPositiveInteger) nthRootRounded: aPositiveInteger.
	^(guess / (1 << p)) asFloat