tests - Float
testFloatPrintString
	"self debug: #testFloatPrintString"
	
	| f r bases |
	f := Float basicNew: 2.
	r := Random new seed: 1234567.
	"printing a Float in base other than 10 is broken if lowercase digits are allowed"
	bases := self areLowercaseDigitsAllowed
		ifTrue: [#(10)]
		ifFalse: [#(2 8 10 16)].
	100
		timesRepeat: [f basicAt: 1 put: (r nextInt: 16r100000000)- 1.
			f basicAt: 2 put: (r nextInt: 16r100000000) - 1.
			bases
				do: [:base | | str |
						str := (String new: 64) writeStream.
						f negative ifTrue: [str nextPut: $-].
						str print: base; nextPut: $r.
						f absPrintExactlyOn: str base: base.
						self assert: (NumberParser parse: str contents) = f]].
	"test big num near infinity"
	10
		timesRepeat: [f basicAt: 1 put: 16r7FE00000 + ((r nextInt: 16r100000) - 1).
			f basicAt: 2 put: (r nextInt: 16r100000000) - 1.
			bases
				do: [:base | | str |
						str := (String new: 64) writeStream.
						f negative ifTrue: [str nextPut: $-].
						str print: base; nextPut: $r.
						f absPrintExactlyOn: str base: base.
						self assert: (NumberParser parse: str contents) = f]].
	"test infinitesimal (gradual underflow)"
	10
		timesRepeat: [f basicAt: 1 put: 0 + ((r nextInt: 16r100000) - 1).
			f basicAt: 2 put: (r nextInt: 16r100000000) - 1.
			bases
				do: [:base | | str |
						str := (String new: 64) writeStream.
						f negative ifTrue: [str nextPut: $-].
						str print: base; nextPut: $r.
						f absPrintExactlyOn: str base: base.
						self assert: (NumberParser parse: str contents) = f]].