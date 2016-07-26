tests - printing
testNumberOfDigits
	
	2 to: 32 do: [:b |
		1 to: 1000//b do: [:n |
			| bRaisedToN |
			bRaisedToN := b raisedTo: n.
			self assert: (bRaisedToN - 1 numberOfDigitsInBase: b) = n.
			self assert: (bRaisedToN numberOfDigitsInBase: b) = (n+1).
			self assert: (bRaisedToN + 1 numberOfDigitsInBase: b) = (n+1).
			
			self assert: (bRaisedToN negated + 1 numberOfDigitsInBase: b) = n.
			self assert: (bRaisedToN negated numberOfDigitsInBase: b) = (n+1).
			self assert: (bRaisedToN negated - 1 numberOfDigitsInBase: b) = (n+1).]].
