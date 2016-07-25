private
generateQandP
	"Generate the two industrial-grade primes, q (160-bits) and p (512-bit) needed to build a key set. Answer the array (q, p, s), where s is the seed that from which q and p were created. This seed is normally discarded, but can be used to verify the key generation process if desired."

	| pBits halfTwoToTheP chunkCount sAndq q twoQ n c w x p s |
	pBits := 512.  "desired size of p in bits"
	halfTwoToTheP := 2 raisedTo: (pBits - 1).
	chunkCount := pBits // 160.

	Transcript show: 'Searching for primes q and p...'; cr.
	[true] whileTrue: [
		sAndq := self generateSandQ.
		Transcript show: '  Found a candidate q.'; cr.
		s := sAndq first.
		q := sAndq last.
		twoQ := q bitShift: 1.
		n := 2.
		c := 0.
		[c < 4096] whileTrue: [
			w := self generateRandomLength: pBits s: s n: n.
			x := w + halfTwoToTheP.
			p := (x - ( x \\ twoQ)) + 1.
			p highBit = pBits ifTrue: [
				Transcript show: '    Testing potential p ', (c + 1) printString, '...'; cr.
				(self isProbablyPrime: p) ifTrue: [
					Transcript show: '  Found p!'; cr.
					^ Array with: q with: p with: s]].
			n := n + chunkCount + 1.
			c := c + 1]].
