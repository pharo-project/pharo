Extension { #name : #Integer }

{ #category : #'*Math-Operations-Extensions' }
Integer >> asWords [
	"SmallInteger maxVal asWords"
	| mils minus three num answer milCount |
	self = 0 ifTrue: [^'zero'].
	mils := #('' ' thousand' ' million' ' billion' ' trillion' ' quadrillion' ' quintillion' ' sextillion' ' septillion' ' octillion' ' nonillion' ' decillion' ' undecillion' ' duodecillion' ' tredecillion' ' quattuordecillion' ' quindecillion' ' sexdecillion' ' septendecillion' ' octodecillion' ' novemdecillion' ' vigintillion').
	num := self.
	minus := ''.
	self < 0 ifTrue: [
		minus := 'negative '.
		num := num negated.
	].
	answer := String new.
	milCount := 1.
	[num > 0] whileTrue: [
		three := (num \\ 1000) threeDigitName.
		num := num // 1000.
		three isEmpty ifFalse: [
			answer isEmpty ifFalse: [
				answer := ', ',answer
			].
			answer := three,(mils at: milCount),answer.
		].
		milCount := milCount + 1.
	].
	^minus,answer
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> benchFib [  "Handy send-heavy benchmark"
	"(result // seconds to run) = approx calls per second"
	" | r t |
	  t := Time millisecondsToRun: [r := 26 benchFib].
	  (r * 1000) // t"
	"138000 on a Mac 8100/100"
	^ self < 2
		ifTrue: [1] 
		ifFalse: [(self-1) benchFib + (self-2) benchFib + 1]

]

{ #category : #'*Math-Operations-Extensions' }
Integer >> benchmark [  "Handy bytecode-heavy benchmark"
	"(500000 // time to run) = approx bytecodes per second"
	"5000000 // (Time millisecondsToRun: [10 benchmark]) * 1000"
	"3059000 on a Mac 8100/100"
    | size flags prime k count |
    size := 8190.
    1 to: self do:
        [:iter |
        count := 0.
        flags := (Array new: size) atAllPut: true.
        1 to: size do:
            [:i | (flags at: i) ifTrue:
                [prime := i+1.
                k := i + prime.
                [k <= size] whileTrue:
                    [flags at: k put: false.
                    k := k + prime].
                count := count + 1]]].
    ^ count
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> decimalDigitAt: anExponent [

	"Return number that represents digit at given decimal position.
		42 digitAt: 2 base: 10 -> 4
		42 digitAt: 1 base: 10 -> 1
	It is always a number or zero:
		1 digitAt: 2 base: 10 -> 0
	Results are not defined non-integer arguments."

	^ self digitAt: anExponent base: 10
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> factorial [
	"Answer the factorial of the receiver."

	self = 0 ifTrue: [^ 1].
	self > 0 ifTrue: [^ self * (self - 1) factorial].
	self error: 'Not valid for negative integers'
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> isPrime [
	"Answer true if the receiver is a prime number. See isProbablyPrime for a probabilistic
	implementation that is much faster for large integers, and that is correct to an extremely
	high statistical level of confidence (effectively deterministic)."
	
	self <= 1 ifTrue: [ ^false ].
	self even ifTrue: [ ^self = 2].
	3 to: self sqrtFloor by: 2 do: [ :each |
		self \\ each = 0 ifTrue: [ ^false ] ].
	^true
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> isProbablyPrime [
	"See isProbablyPrimeWithK:andQ:randomIndex: for the algoritm description."
	
	| k q randomNumbers |
	self <= 1 ifTrue: [ ^false ].
	self even ifTrue: [ ^self = 2 ].
	"Factor self into (2 raisedTo: k) * q + 1, where q odd"
	q := self bitShift: -1.
	k := q lowBit.
	q := q bitShift: 1 - k.
	"Repeat the probabilistic until false (the probability of false negative is null) or until probability is very low."
	
	"Array of pregenerated 25 random numbers. "
	randomNumbers := #(0.816980664067427 0.9940209812456839 0.5106317962103671
		0.18859890764048273 0.7818407135931034 0.39687335928756434
		0.25054954609393587 0.9862212007801148 0.41972151138806785
		0.25944189925652084 0.44000080434605515 0.09351864414919105
		0.7678522154539136 0.292185133924794 0.7555458740124227
		0.4595045267881381 0.8925817282370206 0.621106479606175 
		0.9366027409846908 0.48226772969694237 0.47373301651036975
		0.030808489784043512 0.7982878004192784 0.8230616468112272
		0.19709795629470514).
	
	randomNumbers do: [:rnd | (self isProbablyPrimeWithK: k andQ: q randomIndex: rnd ) ifFalse: [ ^false ] ].
	"The probability of false positive after 25 iterations is less than (1/4 raisedTo: 25) < 1.0e-15"
	^true
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> isProbablyPrimeWithK: k andQ: q randomIndex: aRandomFloat [
	"Algorithm P, probabilistic primality test, from
	Knuth, Donald E. 'The Art of Computer Programming', Vol 2,
	Third Edition, section 4.5.4, page 395, P1-P5 refer to Knuth description..
	Note that this is a Miller Rabin test which may answer false positives (known as pseudoprimes) for at most 1/4 of the possible bases x."

	| x j y minusOne |
	"P1"
	x := ((self - 2) * aRandomFloat "a pregerated random number") asInteger + 1.	
	"P2"
	j := 0.
	y := x raisedTo: q modulo: self.
	minusOne := self - 1.
	
	["P3"
	y = 1 ifTrue: [^j = 0].
	y = minusOne ifTrue: [^true].
	"P4"
	(j := j + 1) < k]
		whileTrue:
			[y := y squared \\ self].
	"P5"
	^false
]

{ #category : #'*Math-Operations-Extensions' }
Integer class >> largePrimesUpTo: maxValue [
	"Compute and return all the prime numbers up to maxValue"
	^Array streamContents:[:s| self largePrimesUpTo: maxValue do:[:prime| s nextPut: prime]]
]

{ #category : #'*Math-Operations-Extensions' }
Integer class >> largePrimesUpTo: max do: aBlock [
	"Evaluate aBlock with all primes up to maxValue.
	The Algorithm is adapted from http://www.rsok.com/~jrm/printprimes.html
	It encodes prime numbers much more compactly than #primesUpTo: 
	38.5 integer per byte (2310 numbers per 60 byte) allow for some fun large primes.
	(all primes up to SmallInteger maxVal can be computed within ~27MB of memory;
	the regular #primesUpTo: would require 4 *GIGA*bytes).
	Note: The algorithm could be re-written to produce the first primes (which require
	the longest time to sieve) faster but only at the cost of clarity."

	| limit flags maskBitIndex bitIndex maskBit byteIndex index primesUpTo2310 indexLimit |
	limit := max asInteger - 1.
	indexLimit := max sqrt truncated + 1.
	"Create the array of flags."
	flags := ByteArray new: (limit + 2309) // 2310 * 60 + 60.
	flags atAllPut: 16rFF. "set all to true"

	"Compute the primes up to 2310"
	primesUpTo2310 := self primesUpTo: 2310.

	"Create a mapping from 2310 integers to 480 bits (60 byte)"
	maskBitIndex := Array new: 2310.
	bitIndex := -1. "for pre-increment"
	maskBitIndex at: 1 put: (bitIndex := bitIndex + 1).
	maskBitIndex at: 2 put: (bitIndex := bitIndex + 1).

	1 to: 5 do:[:i| aBlock value: (primesUpTo2310 at: i)].

	index := 6.
	2 to: 2309 do:[:n|
		[(primesUpTo2310 at: index) < n] 
			whileTrue:[index := index + 1].
		n = (primesUpTo2310 at: index) ifTrue:[
			maskBitIndex at: n+1 put: (bitIndex := bitIndex + 1).
		] ifFalse:[
			"if modulo any of the prime factors of 2310, then could not be prime"
			(n \\ 2 = 0 or:[n \\ 3 = 0 or:[n \\ 5 = 0 or:[n \\ 7 = 0 or:[n \\ 11 = 0]]]]) 
				ifTrue:[maskBitIndex at: n+1 put: 0]
				ifFalse:[maskBitIndex at: n+1 put: (bitIndex := bitIndex + 1)].
		].
	].

	"Now the real work begins...
	Start with 13 since multiples of 2,3,5,7,11 are handled by the storage method;
	increment by 2 for odd numbers only."
	13 to: limit by: 2 do:[:n|
		(maskBit := maskBitIndex at: (n \\ 2310 + 1)) = 0 ifFalse:["not a multiple of 2,3,5,7,11"
			byteIndex := n // 2310 * 60 + (maskBit-1 bitShift: -3) + 1.
			bitIndex := 1 bitShift: (maskBit bitAnd: 7).
			((flags at: byteIndex) bitAnd: bitIndex) = 0 ifFalse:["not marked -- n is prime"
				aBlock value: n.
				"Start with n*n since any integer < n has already been sieved 
				(e.g., any multiple of n with a number k < n has been cleared 
				when k was sieved); add 2 * i to avoid even numbers and
				mark all multiples of this prime. Note: n < indexLimit below
				limits running into LargeInts -- nothing more."
				n < indexLimit ifTrue:[
					index := n * n.
					(index bitAnd: 1) = 0 ifTrue:[index := index + n].
					[index <= limit] whileTrue:[
						(maskBit := maskBitIndex at: (index \\ 2310 + 1)) = 0 ifFalse:[
							byteIndex := (index // 2310 * 60) + (maskBit-1 bitShift: -3) + 1.
							maskBit := 255 - (1 bitShift: (maskBit bitAnd: 7)).
							flags at: byteIndex put: ((flags at: byteIndex) bitAnd: maskBit).
						].
						index := index + (2 * n)].
				].
			].
		].
	].

]

{ #category : #'*Math-Operations-Extensions' }
Integer >> lcm: n [ 
	"Answer the least common multiple of the receiver and n."

	^self // (self gcd: n) * n
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> montgomeryDigitBase [
	"Answer the base used by Montgomery algorithm."
	^1 << self montgomeryDigitLength
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> montgomeryDigitLength [
	"Answer the number of bits composing a digit in Montgomery algorithm.
	Primitive use either 8 or 32 bits digits"
	<primitive: 'primMontgomeryDigitLength' module:'LargeIntegers'>
	^8 "Legacy plugin which did not have this primitive did use 8 bits digits"
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> montgomeryDigitMax [
	"Answer the maximum value of a digit used in Montgomery algorithm."
	
	^1 << self montgomeryDigitLength - 1
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> montgomeryNumberOfDigits [
	"Answer the number of montgomery digits required to represent the receiver."
	^self bytesCount * 8 + (self montgomeryDigitLength - 1) // self montgomeryDigitLength
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> montgomeryRaisedTo: n times: y modulo: m mInvModB: mInv [
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
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> montgomeryTimes: a modulo: m mInvModB: mInv [
	"Answer the result of a Montgomery multiplication
	self * a * (b raisedTo: m montgomeryNumberOfDigits) inv \\ m
	NOTE: it is assumed that:
	self montgomeryNumberOfDigits <= m montgomeryNumberOfDigits
	a montgomeryNumberOfDigits <= m montgomeryNumberOfDigits
	mInv * m \\ b = (-1 \\ b) = (b-1) (this implies m odd)
	where b = self montgomeryDigitBase
	
	Answer nil in case of absent plugin or other failure."
	
	<primitive: 'primMontgomeryTimesModulo' module:'LargeIntegers'>
	^nil
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> nthRoot: aPositiveInteger [
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
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> nthRootRounded: aPositiveInteger [
	"Answer the integer nearest the nth root of the receiver."
	| guess |
	self = 0 ifTrue: [^0].
	self negative
		ifTrue:
			[aPositiveInteger even ifTrue: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ].
			^(self negated nthRootRounded: aPositiveInteger) negated].
	guess := self nthRootTruncated: aPositiveInteger.
	^self * 2 > ((guess + 1 raisedTo: aPositiveInteger) + (guess raisedTo: aPositiveInteger))
		ifTrue: [guess + 1]
		ifFalse: [guess]
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> nthRootTruncated: aPositiveInteger [
	"Answer the integer part of the nth root of the receiver."
	| guess guessToTheNthMinusOne delta |
	self = 0 ifTrue: [^0].
	self negative
		ifTrue:
			[aPositiveInteger even ifTrue: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ].
			^(self negated nthRootTruncated: aPositiveInteger) negated].
	guess := 1 bitShift: self highBitOfMagnitude + aPositiveInteger - 1 // aPositiveInteger.
	[
		guessToTheNthMinusOne := guess raisedTo: aPositiveInteger - 1.
		delta := (guess * guessToTheNthMinusOne - self) // (guessToTheNthMinusOne * aPositiveInteger).
		delta = 0 ] whileFalse:
			[ guess := guess - delta ].
	( (guess := guess - 1) raisedTo: aPositiveInteger) > self  ifTrue:
			[ guess := guess - 1 ].
	^guess
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> primeFactors [
	"Return all primeFactors of myself"
	"( 106260 primeFactors fold: [ :a :b | a * b ]) = 106260"
	
	^ Array streamContents: [ :s | self primeFactorsOn: s ]
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> primeFactorsOn: aStream [
	"Recursively calculate the primefactors of myself and put the factors into the given stream"
	self = 1 
		ifTrue: [ ^ self ].
		
	self even ifTrue: [
		aStream nextPut: 2.
		^ (self / 2) primeFactorsOn: aStream ].
	
	3 to: self sqrtFloor by: 2 do: [ :each |
		self \\ each = 0 
			ifTrue: [ 
				aStream nextPut: each.
				^ (self / each) primeFactorsOn: aStream ]].
	
	aStream nextPut: self.
]

{ #category : #'*Math-Operations-Extensions' }
Integer class >> primesUpTo: max [
	"Return a list of prime integers up to the given integer."
	"Integer primesUpTo: 100"
	^Array streamContents:[:s| self primesUpTo: max do:[:prime| s nextPut: prime]]
]

{ #category : #'*Math-Operations-Extensions' }
Integer class >> primesUpTo: max do: aBlock [
	"Compute aBlock with all prime integers up to the given integer."
	"Integer primesUpTo: 100"

	| limit flags prime k |
	limit := max asInteger - 1.
	"Fall back into #largePrimesUpTo:do: if we'd require more than 100k of memory; 
	the alternative will only requre 1/154th of the amount we need here and is almost as fast."
	limit > 25000 ifTrue:[^self largePrimesUpTo: max do: aBlock].
	flags := (Array new: limit) atAllPut: true.
	1 to: limit - 1 do: [:i |
		(flags at: i) ifTrue: [
			prime := i + 1.
			k := i + prime.
			[k <= limit] whileTrue: [
				flags at: k put: false.
				k := k + prime].
			aBlock value: prime]].

]

{ #category : #'*Math-Operations-Extensions' }
Integer >> printStringRoman [
	| stream integer |
	stream := String new writeStream.
	integer := self negative ifTrue: [stream nextPut: $-. self negated] ifFalse: [self].
	integer // 1000 timesRepeat: [stream nextPut: $M].
	integer
		romanDigits: 'MDC' for: 100 on: stream;
		romanDigits: 'CLX' for: 10 on: stream;
		romanDigits: 'XVI' for: 1 on: stream.
	^stream contents
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> raisedTo: n modulo: m [
	"Answer the modular exponential.
	Note: this implementation is optimized for case of large integers raised to large powers."
	| a s mInv |
	n = 0 ifTrue: [^1].
	(self >= m or: [self < 0]) ifTrue: [^self \\ m raisedTo: n modulo: m].
	n < 0 ifTrue: [^(self reciprocalModulo: m) raisedTo: n negated modulo: m].
	(n < 4096 or: [m even])
		ifTrue:
			["Overhead of Montgomery method might cost more than naive divisions, use naive"
			^self slidingLeftRightRaisedTo: n modulo: m].
	
	mInv := self montgomeryDigitBase - ((m bitAnd: self montgomeryDigitMax) reciprocalModulo: self montgomeryDigitBase).
 
	"Initialize the result to R=self montgomeryDigitModulo raisedTo: m montgomeryNumberOfDigits"
	a := (1 bitShift: m montgomeryNumberOfDigits * m montgomeryDigitLength) \\ m.
	
	"Montgomerize self (multiply by R)"
	(s := self montgomeryTimes: (a*a \\ m) modulo: m mInvModB: mInv)
		ifNil:
			["No Montgomery primitive available ? fallback to naive divisions"
			^self slidingLeftRightRaisedTo: n modulo: m].

	"Exponentiate self*R"
	a := s montgomeryRaisedTo: n times: a modulo: m mInvModB: mInv.

	"Demontgomerize the result (divide by R)"
	^a montgomeryTimes: 1 modulo: m mInvModB: mInv
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> raisedToFraction: aFraction [
	| root |
	root := self nthRootTruncated: aFraction denominator.
	(root raisedToInteger: aFraction denominator) = self ifTrue: [^root raisedToInteger: aFraction numerator].
	^super raisedToFraction: aFraction
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> raisedToInteger: exp modulo: m [
	(exp = 0) ifTrue: [^ 1].
	exp even
		ifTrue: [^ (self raisedToInteger: (exp // 2) modulo: m) squared \\ m]
		ifFalse: [^ (self * (self raisedToInteger: (exp - 1) modulo: m)) \\ m].
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> romanDigits: digits for: base on: aStream [
	| n |
	n := self \\ (base * 10) // base.
	n = 9 ifTrue: [^ aStream nextPut: digits last; nextPut: digits first].
	n = 4 ifTrue: [^ aStream nextPut: digits last; nextPut: digits second].
	n > 4 ifTrue: [aStream nextPut: digits second].
	n \\ 5 timesRepeat: [aStream nextPut: digits last]
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> slidingLeftRightRaisedTo: n modulo: m [
	"Private - compute (self raisedTo: n) \\ m,
	Note: this method has to be fast because it is generally used with large integers in cryptography.
	It thus operate on exponent bits from left to right by packets with a sliding window rather than bit by bit (see below)."
	
	| pow j k w index oddPowersOfSelf square |
	
	"Precompute powers of self for odd bit patterns xxxx1 up to length w + 1.
	The width w is chosen with respect to the total bit length of n,
	such that each bit pattern will on average be encoutered P times in the whole bit sequence of n.
	This costs (2 raisedTo: w) multiplications, but more will be saved later (see below)."
	k := n highBit.
	w := (k highBit - 1 >> 1 min: 16) max: 1.
	oddPowersOfSelf := Array new: 1 << w.
	oddPowersOfSelf at: 1 put: (pow := self).
	square := self * self \\ m.
	2 to: oddPowersOfSelf size do: [:i | pow := oddPowersOfSelf at: i put: pow * square \\ m].
	
	"Now exponentiate by searching precomputed bit patterns with a sliding window"
	pow := 1.
	[k > 0]
		whileTrue:
			[pow := pow * pow \\ m.
			"Skip bits set to zero (the sliding window)"
			(n bitAt: k) = 0
				ifFalse:
					["Find longest odd bit pattern up to window length (w + 1)"
					j := k - w max: 1.
					[j < k and: [(n bitAt: j) = 0]] whileTrue: [j := j + 1].
					"We found an odd bit pattern of length k-j+1;
					perform the square powers for each bit
					(same cost as bitwise algorithm);
					compute the index of this bit pattern in the precomputed powers."
					index := 0.
					[k > j] whileTrue:
						[pow := pow * pow \\ m.
						index := index << 1 + (n bitAt: k).
						k := k - 1].
					"Perform a single multiplication for the whole bit pattern.
					This saves up to (k-j) multiplications versus a naive algorithm operating bit by bit"
					pow := pow * (oddPowersOfSelf at: index + 1) \\ m].
			k := k - 1].
	^pow
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> sqrt [
	"Answer the square root of the receiver."

	| selfAsFloat floatResult guess |
	selfAsFloat := self asFloat.
	floatResult := selfAsFloat sqrt.

	floatResult isInfinite ifFalse: [
		guess := floatResult truncated.

		"If got an exact answer, answer it. Otherwise answer float approximate answer."
		guess squared = self
			ifTrue: [ ^ guess ]].

	"In this case, maybe it failed because we are such a big integer that the Float method becomes
	inexact, even if we are a whole square number. So, try the slower but more general method"
	selfAsFloat >= Float maxExactInteger asFloat squared
		ifTrue: [
			guess := self sqrtFloor.
			guess squared = self ifTrue: [
				^guess ].
			
			"Nothing else can be done. No exact answer means answer must be a Float.
			Answer the best we have which is the rounded sqrt."
			guess := (self * 4) sqrtFloor.
			^(guess // 2 + (guess \\ 2)) asFloat].

	"We need an approximate result"
	^floatResult
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> sqrtFloor [
	"Return the integer part of the square root of self"

	| guess guessSquared delta |
	guess := 1 bitShift: self highBit + 1 // 2.
	[
		guessSquared := guess * guess.
		delta := guessSquared - self // (guess bitShift: 1).
		delta = 0 ] whileFalse: [
			guess := guess - delta ].
	guessSquared = self ifFalse: [ guess := guess - 1 ].
	^guess
]

{ #category : #'*Math-Operations-Extensions' }
Integer >> take: kk [
	"Return the number of combinations of (self) elements taken kk at a time.  
	For 6 take 3, this is 6*5*4 / (1*2*3).  Zero outside of Pascal's triangle.  Use a trick to go faster."
	" 6 take: 3  "

	| num denom |
	kk < 0 ifTrue: [^ 0].
	kk > self ifTrue: [^ 0].
	num := 1.
	self to: (kk max: self-kk) + 1 by: -1 do: [:factor | num := num * factor].
	denom := 1.
	1 to: (kk min: self-kk) do: [:factor | denom := denom * factor].
	^ num // denom
]
