render thread
createRenderThread
	RenderThreadProcess ifNotNil: [ RenderThreadProcess terminate ].
	
	RenderThreadQueue := AtomicSharedQueue new.
	OperationAvailableSemaphore := Semaphore new.
	
	RegisteredWindows := WeakSet new.
	RegisteredWindowsMutex := Semaphore forMutualExclusion.
	
	RenderThreadProcess := [ self renderThreadProcess ] forkAt: Processor userSchedulingPriority.
	RenderThreadProcess name:  'OS Window Renderer Loop'; resume.
