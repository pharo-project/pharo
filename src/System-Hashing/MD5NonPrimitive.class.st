"
This class implements the MD5 128-bit one-way hash function.  It relies on
the ThirtyTwoBitRegister class supplied as part of the ""Digital Signatures""
functionality included in Squeak 2.7.  As of this date (1/20/2000), the
U.S. Government has lifted many of the previous restrictions on the export
of encryption software, but you should check before exporting anything
including this code.  MD5 is commonly used for some secure Internet
protocols, including authentication in HTTP, which is why I wrote it.

Submitted by Duane Maxwell


"
Class {
	#name : #MD5NonPrimitive,
	#superclass : #MD5,
	#classVars : [
		'ABCDTable',
		'IndexTable',
		'ShiftTable',
		'SinTable'
	],
	#category : #'System-Hashing-MD5'
}

{ #category : #'class initialization' }
MD5NonPrimitive class >> initialize [
	"MD5NonPrimitive initialize"
	"Obscure fact: those magic hex numbers that are hard to type in correctly are
	actually the result of a simple trigonometric function and are therefore
	easier to compute than proofread.  Laziness is sometimes a virtue."
	| c |
	c := 2 raisedTo: 32.
	SinTable := Array new: 64.
	1 
		to: 64
		do: 
			[ :i | 
			SinTable 
				at: i
				put: (ThirtyTwoBitRegister new load: (c * i sin abs) truncated) ].
	ShiftTable := { 
		#(7 12 17 22 ).
		#(5 9 14 20 ).
		#(4 11 16 23 ).
		#(6 10 15 21 )
	 }.
	IndexTable := { 
		#(
			1
			2
			3
			4
			5
			6
			7
			8
			9
			10
			11
			12
			13
			14
			15
			16
		).
		#(
			2
			7
			12
			1
			6
			11
			16
			5
			10
			15
			4
			9
			14
			3
			8
			13
		).
		#(
			6
			9
			12
			15
			2
			5
			8
			11
			14
			1
			4
			7
			10
			13
			16
			3
		).
		#(
			1
			8
			15
			6
			13
			4
			11
			2
			9
			16
			7
			14
			5
			12
			3
			10
		)
	 }.
	ABCDTable := { 
		#(1 2 3 4 ).
		#(4 1 2 3 ).
		#(3 4 1 2 ).
		#(2 3 4 1 )
	 }
]

{ #category : #'private-functions' }
MD5NonPrimitive >> fX: x Y: y Z: z [
	" compute 'xy or (not x)z'"
	^ x copy bitAnd: y; bitOr: (x copy bitInvert; bitAnd: z)

	
]

{ #category : #'private-functions' }
MD5NonPrimitive >> ffA: a B: b C: c D: d M: m S: s T: t [
	"compute a = b + ((a + f(b,c,d) + m + t) <<< s)"
	^ a += (self fX: b Y: c Z: d); += m; += t; leftRotateBy: s; += b.

]

{ #category : #'private-buffers' }
MD5NonPrimitive >> finalValue [
	"Concatenate the state values to produce the 128-bite result"
	^ (state at: 1) asByteArray, (state at: 2) asByteArray, (state at: 3) asByteArray, (state at: 4) asByteArray
]

{ #category : #'private-functions' }
MD5NonPrimitive >> gX: x Y: y Z: z [
	" compute 'xz or y(not z)'"
	^ x copy bitAnd: z; bitOr: (z copy bitInvert; bitAnd: y)

	
]

{ #category : #'private-functions' }
MD5NonPrimitive >> ggA: a B: b C: c D: d M: m S: s T: t [
	"compute a = b + ((a + g(b,c,d) + m + t) <<< s)"
	^ a += (self gX: b Y: c Z: d); += m; += t; leftRotateBy: s; += b.

]

{ #category : #'private-functions' }
MD5NonPrimitive >> hX: x Y: y Z: z [
	" compute 'x xor y xor z'"
	^ x copy bitXor: y; bitXor: z

	
]

{ #category : #'private-functions' }
MD5NonPrimitive >> hhA: a B: b C: c D: d M: m S: s T: t [
	"compute a = b + ((a + h(b,c,d) + m + t) <<< s)"
	^ a += (self hX: b Y: c Z: d); += m; += t; leftRotateBy: s; += b.

]

{ #category : #'private-functions' }
MD5NonPrimitive >> iX: x Y: y Z: z [
	" compute 'y xor (x or (not z))'"
	^ y copy bitXor: (z copy bitInvert; bitOr: x)

]

{ #category : #'private-functions' }
MD5NonPrimitive >> iiA: a B: b C: c D: d M: m S: s T: t [
	"compute a = b + ((a + i(b,c,d) + m + t) <<< s)"
	^ a += (self iX: b Y: c Z: d); += m; += t; leftRotateBy: s; += b.

]

{ #category : #initialization }
MD5NonPrimitive >> initialize [
	"Some magic numbers to get the process started"
	state := OrderedCollection newFrom: { 
			(ThirtyTwoBitRegister new load: 1732584193).
			(ThirtyTwoBitRegister new load: 4023233417).
			(ThirtyTwoBitRegister new load: 2562383102).
			(ThirtyTwoBitRegister new load: 271733878)
		 }
]

{ #category : #'private-buffers' }
MD5NonPrimitive >> processBuffer: aByteArray [ 
	"Process a 64-byte buffer"
	| saveState data |
	saveState := state collect: [ :item | item copy ].
	data := Array new: 16.
	1 
		to: 16
		do: 
			[ :index | 
			data 
				at: index
				put: (ThirtyTwoBitRegister new 
						reverseLoadFrom: aByteArray
						at: index * 4 - 3) ].
	self rounds: data.
	1 
		to: 4
		do: [ :index | (state at: index) += (saveState at: index) ]
]

{ #category : #'private-rounds' }
MD5NonPrimitive >> round: data selector: selector round: round [ 
	"Do one round with the given function"
	| shiftIndex template abcd |
	1 
		to: 16
		do: 
			[ :i | 
			shiftIndex := (i - 1) \\ 4 + 1.
			abcd := ABCDTable at: shiftIndex.
			template := { 
				(abcd at: 1).
				(abcd at: 2).
				(abcd at: 3).
				(abcd at: 4).
				((IndexTable at: round) at: i).
				((ShiftTable at: round) at: shiftIndex).
				(SinTable at: (round - 1) * 16 + i)
			 }.
			self 
				step: data
				template: template
				selector: selector ]
]

{ #category : #'private-rounds' }
MD5NonPrimitive >> rounds: data [
	"Perform the four rounds with different functions"
	#(
	ffA:B:C:D:M:S:T:
	ggA:B:C:D:M:S:T:
	hhA:B:C:D:M:S:T:
	iiA:B:C:D:M:S:T:
	) doWithIndex: [ :selector :index |
		self round: data selector: selector round: index.]

]

{ #category : #'private-functions' }
MD5NonPrimitive >> step: data template: item selector: selector [ 
	"Perform one step in the round"
	| args |
	args := { 
		(state at: (item at: 1)).
		(state at: (item at: 2)).
		(state at: (item at: 3)).
		(state at: (item at: 4)).
		(data at: (item at: 5)).
		(item at: 6).
		(item at: 7)
	 }.
	self 
		perform: selector
		withArguments: args
]
