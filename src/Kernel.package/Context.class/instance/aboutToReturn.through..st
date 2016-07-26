private
aboutToReturn: result through: firstUnwindContext 
	"Called from VM when an unwindBlock is found between self and its home.
	 Return to home's sender, executing unwind blocks on the way."

	self methodReturnContext return: result through: firstUnwindContext