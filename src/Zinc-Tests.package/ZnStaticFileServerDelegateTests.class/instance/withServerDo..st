private
withServerDo: block
	| server staticFileServerDelegate |
	server := ZnServer on: 1700 + 32 atRandom.
	[ 
		(staticFileServerDelegate := ZnStaticFileServerDelegate new)
			prefixFromString: 'local-files'; 
			directory: ZnFileSystemUtils defaultDirectory;
			mimeTypeExpirations: ZnStaticFileServerDelegate defaultMimeTypeExpirations. 
		server delegate: staticFileServerDelegate.
		server start.
		self 
			assert: server isRunning & server isListening
			description: ('Failed to start server on port {1}. Is there one already?' format: { server port }).
		block cull: server
	] 
		ensure: [ server stop ]