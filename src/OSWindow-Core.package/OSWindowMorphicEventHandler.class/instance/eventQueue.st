accessing
eventQueue
	"using lazy-initialize here to deal with bootstrapping issues,
	since existing instances originally has no queue"
	^ eventQueue ifNil: [ eventQueue := WaitfreeQueue new ]