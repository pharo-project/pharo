initialization
method: aMethod

	| flagByte |

	data := size := nil.
	method := aMethod.
	flagByte := method at: (method size).

	"trailer kind encoded in 6 high bits of last byte"
	kind := self class trailerKinds at: 1+(flagByte>>2).

	"decode the trailer bytes, inline some common types to speed up decoding"
	kind == #SourcePointer
		ifTrue: [ self decodeSourcePointer ]
	ifFalse: [ kind == #VarLengthSourcePointer
		ifTrue: [ self decodeVarLengthSourcePointer ]
	ifFalse: [ kind == #NoTrailer
			ifTrue: [ self decodeNoTrailer ]
	ifFalse: [ 
		"slow but general decoding using perform"
		self perform: ('decode' , kind) asSymbol ]]].
