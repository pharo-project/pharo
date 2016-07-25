random seed
makeUnixSeed
	
	^ [FileStream readOnlyFileNamed: '/dev/urandom' do: [ :strm | 
			strm binary.
			strm converter: Latin1TextConverter new.
			Integer
				byte1: strm next
				byte2: strm next
				byte3: strm next
				byte4: strm next. ] ] on: FileException do: [ nil ].
