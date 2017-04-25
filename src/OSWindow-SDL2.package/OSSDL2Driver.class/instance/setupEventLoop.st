events-processing
setupEventLoop
	EventLoopProcess ifNotNil: [ EventLoopProcess terminate ].
	EventLoopProcess := self class isVMDisplayUsingSDL2 ifTrue: [ 
		[ self eventLoopProcessWithVMWindow ] 
		forkAt: Processor lowIOPriority
	] ifFalse: [ 
		self class hasPlugin ifTrue: [ 
			[ self eventLoopProcessWithPlugin ] 
			forkAt: Processor lowIOPriority ]
		ifFalse: [ 
			[ self eventLoopProcessWithoutPlugin ] 
			forkAt: Processor lowIOPriority ].
	].

	EventLoopProcess
		name: 'SDL2 Event loop';
		resume