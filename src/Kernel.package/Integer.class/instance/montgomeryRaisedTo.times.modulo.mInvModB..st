private
montgomeryRaisedTo: n times: y modulo: m mInvModB: mInv
	"Private - do a Montgomery exponentiation of self modulo m.
	The operation is equivalent to (self/y raisedTo: n)*y \\ m,
	with y is (b raisedTo: m montgomeryNumberOfDigits),
	with (m bitAnd: b-1) * mInv \\ b = (b-1)
	with b = self montgomeryDigitBase (either 1<<8 or 1<<32)"
	
	| pow j k w index oddPowersOfSelf square |
	
	"Precompute powers of self for odd bit patterns xxxx1 up to length w + 1.
	The width w is chosen with respect to the total bit length of n,
	such that each bit pattern will on average be encoutered P times in the whole bit sequence of n.
	This costs (2 raisedTo: w) multiplications, but more will be saved later (see below)."
	k := n highBit.
	w := (k highBit - 1 >> 1 min: 16) max: 1.
	oddPowersOfSelf := Array new: 1 << w.
	oddPowersOfSelf at: 1 put: (pow := self).
	square := self montgomeryTimes: self modulo: m mInvModB: mInv.
	2 to: oddPowersOfSelf size do: [:i | pow := oddPowersOfSelf at: i put: (pow montgomeryTimes: square modulo: m mInvModB: mInv)].
	
	"Now exponentiate by searching precomputed bit patterns with a sliding window"
	pow := y.
	[k > 0]
		whileTrue:
			[pow := pow montgomeryTimes: pow modulo: m mInvModB: mInv.
			"Skip bits set to zero (the sliding window)"
			(n bitAt: k) = 0
				ifFalse:
					["Find longest odd bit pattern up to window length (w + 1)"
					j := k - w max: 1.
					[j < k and: [(n bitAt: j) = 0]] whileTrue: [j := j + 1].
					"We found a bit pattern of length k-j+1;
					perform the square powers for each bit
					(same cost as bitwise algorithm);
					compute the index of this bit pattern in the precomputed powers."
					index := 0.
					[k > j] whileTrue:
						[pow := pow montgomeryTimes: pow modulo: m mInvModB: mInv.
						index := index << 1 + (n bitAt: k).
						k := k - 1].
					"Perform a single multiplication for the whole bit pattern.
					This saves up to (k-j) multiplications versus a naive algorithm operating bit by bit"
					pow := pow montgomeryTimes: (oddPowersOfSelf at: index + 1) modulo: m mInvModB: mInv].
			k := k - 1].
	^pow