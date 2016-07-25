initialize-release
asciiResponse256k
	message := ZnResponse ok: 
		(ZnEntity 
			with: (self randomAsciiString: 256 * 1024) 
			type: 'text/plain;charset=ascii')