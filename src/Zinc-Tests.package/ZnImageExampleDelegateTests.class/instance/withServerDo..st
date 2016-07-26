private
withServerDo: block
	| server |
	server := ZnServer on: 1700 + 32 atRandom.
	[ 
		server start.
		self 
			assert: server isRunning & server isListening
			description: ('Failed to start server on port {1}. Is there one already?' format: { server port }).
		server delegate: ZnImageExampleDelegate new.
		block cull: server
	] 
		ensure: [ server stop ]