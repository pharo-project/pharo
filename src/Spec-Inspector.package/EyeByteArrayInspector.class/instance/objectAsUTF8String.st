converting
objectAsUTF8String
	^ String
		streamContents: [ :out | 
			| in encoder |
			encoder := ZnUTF8Encoder new.
			in := self object readStream.
			[ in atEnd ] whileFalse: [ 
				out nextPut: (encoder nextFromStream: in) ] ]
		limitedTo: 5000