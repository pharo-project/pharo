testing
testNextPutAllStartingAtToStream
	| encoder |
	encoder := ZnUTF8Encoder new.
	#( 'ccc' 'ççç' 'c' 'ç' 'çc' 'cç' 'çç' ) do: [ :each |
		#( ( '' '' ) ( 'ABC' '' ) ( '' 'ABC' ) ( 'ABC' 'ABC' ) 
			( 'AéC' '' ) ( '' 'AèC' ) ( 'AéC' 'AèC' ) 
			( 'AXC' 'AèC' ) ( 'AéC' 'AXC' ) 
			( 'PRE' 'ç' ) ) do: [ :extra |
				| prefix postfix string bytes |
				prefix := extra first.
				postfix := extra last.
				string := prefix, each, postfix.
				bytes := ByteArray streamContents: [ :out |
					encoder next: each size putAll: string startingAt: prefix size + 1 toStream: out ].
				self assert: (encoder decodeBytes: bytes) equals: each ] ]