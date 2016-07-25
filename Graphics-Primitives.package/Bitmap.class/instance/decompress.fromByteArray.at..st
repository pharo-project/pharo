filing
decompress: bm fromByteArray: ba at: index 
	"Decompress the body of a byteArray encoded by compressToByteArray (qv)...
	The format is simply a sequence of run-coded pairs, {N D}*.
		N is a run-length * 4 + data code.
		D, the data, depends on the data code...
			0	skip N words, D is absent
				(could be used to skip from one raster line to the next)
			1	N words with all 4 bytes = D (1 byte)
			2	N words all = D (4 bytes)
			3	N words follow in D (4N bytes)
		S and N are encoded as follows (see decodeIntFrom:)...
			0-223	0-223
			224-254	(0-30)*256 + next byte (0-7935)
			255		next 4 bytes"
	"NOTE:  If fed with garbage, this routine could read past the end of ba, but it should fail before writing past the ned of bm."
	| i code n anInt data end k pastEnd |
	<primitive: 'primitiveDecompressFromByteArray' module: 'MiscPrimitivePlugin'>
	
	<var: #bm declareC: 'int *bm'>
	<var: #ba declareC: 'unsigned char *ba'>
	
	i := index.	"byteArray read index"
	end := ba size.
	k := 1.	"bitmap write index"
	pastEnd := bm size + 1.
	[ i <= end ] whileTrue: 
		[ "Decode next run start N"
		anInt := ba at: i.
		i := i + 1.
		anInt <= 223 ifFalse: 
			[ anInt <= 254 
				ifTrue: 
					[ anInt := (anInt - 224) * 256 + (ba at: i).
					i := i + 1 ]
				ifFalse: 
					[ anInt := 0.
					1 
						to: 4
						do: 
							[ :j | 
							anInt := (anInt bitShift: 8) + (ba at: i).
							i := i + 1 ] ] ].
		n := anInt >> 2.
		k + n > pastEnd ifTrue: [ ^ self primitiveFail ].
		code := anInt bitAnd: 3.
		code = 0 ifTrue: 
			[ "skip"
			 ].
		code = 1 ifTrue: 
			[ "n consecutive words of 4 bytes = the following byte"
			data := ba at: i.
			i := i + 1.
			data := data bitOr: (data bitShift: 8).
			data := data bitOr: (data bitShift: 16).
			1 
				to: n
				do: 
					[ :j | 
					bm 
						at: k
						put: data.
					k := k + 1 ] ].
		code = 2 ifTrue: 
			[ "n consecutive words = 4 following bytes"
			data := 0.
			1 
				to: 4
				do: 
					[ :j | 
					data := (data bitShift: 8) bitOr: (ba at: i).
					i := i + 1 ].
			1 
				to: n
				do: 
					[ :j | 
					bm 
						at: k
						put: data.
					k := k + 1 ] ].
		code = 3 ifTrue: 
			[ "n consecutive words from the data..."
			1 
				to: n
				do: 
					[ :m | 
					data := 0.
					1 
						to: 4
						do: 
							[ :j | 
							data := (data bitShift: 8) bitOr: (ba at: i).
							i := i + 1 ].
					bm 
						at: k
						put: data.
					k := k + 1 ] ] ]