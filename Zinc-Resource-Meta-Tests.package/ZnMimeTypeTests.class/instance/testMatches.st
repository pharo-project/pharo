testing
testMatches
	#(
		( 'text/plain' 'text/*' )
		( 'text/plain' '*/*' )
		( 'text/plain;charset=utf-8' 'text/*' ) 
		( 'text/plain;charset=utf-8' 'text/plain' )
		( 'text/plain' 'text/plain;charset=utf-8' )
		( 'text/plain;charset=utf-8' 'text/plain;charset=ascii' ) ) do: [ :each |
			self deny: each first asZnMimeType = each second asZnMimeType.
			self assert: (each first asZnMimeType matches: each second asZnMimeType) ]