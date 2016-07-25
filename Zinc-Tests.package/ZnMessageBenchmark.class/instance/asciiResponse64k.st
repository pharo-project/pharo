initialize-release
asciiResponse64k
	message := ZnResponse ok: 
		(ZnEntity 
			with: (self randomAsciiString: 64 * 1024) 
			type: 'text/plain;charset=ascii')