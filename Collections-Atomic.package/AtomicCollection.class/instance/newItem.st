utils
newItem
	"override in subclass, if you need to instantiate queue items of different class or initialize them in some specific way"
	^ AtomicQueueItem new