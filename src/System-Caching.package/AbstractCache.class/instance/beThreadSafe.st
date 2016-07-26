initialize
beThreadSafe
	"Configure me so that I can be safely used from multiple threads/processes
	during important operations. Note that this slows down these operations."
	
	access := Monitor new