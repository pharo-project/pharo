converting
hex
    " an alternate implementation was | result stream |
	result := String new: self size * 2.
	stream := result writeStream.
	1 to: self size do: [ :ix | |each|
		each := self at: ix.
		stream
			nextPut: ('0123456789ABCDEF' at: each // 16 + 1);
			nextPut: ('0123456789ABCDEF' at: each \\ 16 + 1)].
    ^ result"
	
	"Answer a hexa decimal representation of the receiver"
	| string v index map |
	map := '0123456789abcdef'.
	string := String new: self size * 2. "hex"
	index := 0.
	1 to: self size do: [ :i |
		v := self at: i.
		string at: (index := index + 1) put: (map at: (v bitShift: -4) + 1).
		string at: (index := index + 1) put: (map at: (v bitAnd: 15) + 1)].
	^string