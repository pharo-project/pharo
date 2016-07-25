initialize-release
bePerform: aSelector
	"Match any single character that answers true  to this message."

	self predicate: 
		[:char | 
		RxParser doHandlingMessageNotUnderstood: [char perform: aSelector]]