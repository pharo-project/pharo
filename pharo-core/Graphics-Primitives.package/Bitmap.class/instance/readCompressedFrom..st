filing
readCompressedFrom: strm 
	"Decompress an old-style run-coded stream into this bitmap:
		[0 means end of runs]
		[n = 1..127] [(n+3) copies of next byte]
		[n = 128..191] [(n-127) next bytes as is]
		[n = 192..255] [(n-190) copies of next 4 bytes]"
	| n byte out outBuff bytes |
	out := (outBuff := ByteArray new: self size * 4) writeStream.
	[ (n := strm next) > 0 ] whileTrue: 
		[ (n 
			between: 1
			and: 127) ifTrue: 
			[ byte := strm next.
			1 
				to: n + 3
				do: [ :i | out nextPut: byte ] ].
		(n 
			between: 128
			and: 191) ifTrue: 
			[ 1 
				to: n - 127
				do: [ :i | out nextPut: strm next ] ].
		(n 
			between: 192
			and: 255) ifTrue: 
			[ bytes := (1 to: 4) collect: [ :i | strm next ].
			1 
				to: n - 190
				do: [ :i | bytes do: [ :b | out nextPut: b ] ] ] ].
	out position = outBuff size ifFalse: [ self error: 'Decompression size error' ].
	"Copy the final byteArray into self"
	self copyFromByteArray: outBuff