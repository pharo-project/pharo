initialize-release
asciiResponse8k
	message := ZnResponse ok: 
		(ZnEntity 
			with: (self randomAsciiString: 8 * 1024) 
			type: 'text/plain;charset=ascii')