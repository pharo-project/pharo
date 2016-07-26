events-processing
setupEventLoop
	EventLoopProcess ifNotNil: [ EventLoopProcess terminate ].
	EventLoopProcess := self class hasPlugin
		ifTrue:
			[ [ self eventLoopProcessWithPlugin ] forkAt: Processor lowIOPriority ]
		ifFalse:
			[ [ self eventLoopProcessWithoutPlugin ] forkAt: Processor lowIOPriority ].
	EventLoopProcess
		name: 'SDL2 Event loop';
		resume