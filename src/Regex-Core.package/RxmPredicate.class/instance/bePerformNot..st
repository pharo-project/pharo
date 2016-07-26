initialize-release
bePerformNot: aSelector
	"Match any single character that answers false to this message."

	self predicate: 
		[:char | 
		RxParser doHandlingMessageNotUnderstood: [(char perform: aSelector) not]]