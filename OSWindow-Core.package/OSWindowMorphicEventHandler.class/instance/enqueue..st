events
enqueue: aMorphicEvent
	"Put morphic event into receiver's internal queue. 
	All events in queue will be processed later, by UI process, once it will get there."
	
	self eventQueue nextPut: aMorphicEvent.