accessing
removeAll
	"
	This message makes no sense in concurrent environment. There is no way to guarantee that when this method returns to sender, the queue will remain empty,
	because at any moment other process may interrupt current process and put new item(s) to queue.

	Therefore we just flush the queue and hope for the best"
	
	self flush